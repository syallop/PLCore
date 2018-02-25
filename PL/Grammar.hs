{-# LANGUAGE RankNTypes, OverloadedStrings, GADTs #-}
module PL.Grammar where

import PLGrammar
import PLGrammar.Iso
import PLPrinter
import PLParser
import PLParser.Cursor

import qualified PLGrammar as G
import qualified PLParser  as P

import qualified Data.List as List
import qualified Data.Text as Text

import Data.Text (Text)
import Data.Char

import Control.Applicative
import Control.Monad
import Data.Monoid

-- A Parser that accepts that grammar if the Iso also succeeds.
isoMapParser
  :: Show a
  => Iso a b
  -> G.Grammar a
  -> Parser b
isoMapParser iso@(Iso labels _ _) gr =
  let Parser p = toParser gr
   in Parser $ \cur0 -> case p cur0 of
        ParseSuccess a cur1
          -> case parseIso iso a of
               Nothing
                 -> ParseFailure [(ExpectLabel (Text.intercalate "." labels) $ grammarExpects gr, cur0)] cur1 -- cur1

               Just b
                 -> ParseSuccess b cur1

        ParseFailure failures cur1
          -> ParseFailure failures cur1



-- | Tuple the result of two successive parsers.
productMapParser :: Parser a -> Parser b -> Parser (a,b)
productMapParser fa fb = (,) <$> fa <*> fb

-- | Convert a Grammar to a Parser that accepts it.
toParser :: G.Grammar a -> Parser a
toParser grammar = case grammar of
  -- A single character if one is available.
  G.GAnyChar
    -> takeChar

  -- A token of text stopping at the first whitespace.
  G.GAnyText
    -> takeWhile1 (Predicate (not . isSpace) $ ExpectAnything)

  -- Return the value.
  G.GPure a
    -> pure a

  -- Fail with no Expectations.
  G.GEmpty
    -> empty

  -- If the left fails, try the right as if no input had been consumed.
  G.GAlt g0 g1
    -> toParser g0 <|> toParser g1

  -- Parse the grammar if the iso succeeds.
  G.GIsoMap iso ga
    -> isoMapParser iso ga

  -- | Tuple the result of two successive parsers.
  G.GProductMap ga gb
    -> productMapParser (toParser ga) (toParser gb)

  -- Enhance a failing parse with a given Expect label.
  G.GLabel l g
    -> let Parser f = toParser g
        in Parser $ \cur0 -> case f cur0 of
             ParseFailure failures cur1
               -> ParseFailure (map (\(e,c) -> (ExpectLabel l e,c)) failures) cur1
             s -> s

  G.GTry g0
    -> P.try . toParser $ g0


-- | A Grammar's parser expected to see:
grammarExpects :: forall a. Show a => Grammar a -> Expected
grammarExpects g0 = case g0 of
  -- Expected a single character.
  GAnyChar
    -> ExpectN 1 ExpectAnything

  -- Expected a specific thing.
  GPure a
    -> ExpectOneOf [Text.pack . show $ a]

  -- Expected to fail.
  GEmpty
    -> ExpectOneOf []

  -- Expected one or the other.
  GAlt l r
    -> ExpectEither (grammarExpects l) (grammarExpects r)

  -- Expects something AND a predicate to succeed.
  -- TODO: Capture this desired predicate?
  GIsoMap (Iso labels _ _) g1
    -> ExpectPredicate (Text.intercalate "." labels) . Just . grammarExpects $ g1

  -- Expected one thing and then another.
  -- TODO: Express what we wanted after the immediate thing?
  GProductMap g1 g2
    -> ExpectPredicate "THEN" . Just $ (ExpectPredicate "THEN" . Just . grammarExpects $ g1)

  GLabel l g
    -> ExpectPredicate l Nothing

  GTry g
    -> ExpectPredicate "TRY" . Just $ grammarExpects g

isoMapPrinter :: Iso a b -> Printer a -> Printer b
isoMapPrinter iso (Printer p) = Printer $ printIso iso >=> p

toPrinter :: Grammar a -> Printer a
toPrinter grammar = case grammar of
  GAnyChar
    -> anyCharPrinter

  GAnyText
    -> anyTextPrinter

  GPure a
    -> purePrinter a

  GEmpty
    -> emptyPrinter

  GAlt g0 g1
    -> altPrinter (toPrinter g0) (toPrinter g1)

  GIsoMap iso ga
    -> isoMapPrinter iso (toPrinter ga)

  GProductMap ga gb
    -> productMapPrinter (toPrinter ga) (toPrinter gb)

  GLabel _label g
    -> toPrinter g

describeGrammar :: Show a => Grammar a -> Doc
describeGrammar gr = case gr of
  GAnyChar
    -> text "."

  GAnyText
    -> text "*"

  GPure a
    -> mconcat
         [text "( "
         ,text (Text.pack . show $ a)
         ,text " )"
         ]
  GEmpty
    -> text "()"

  GAlt g0 g1
    -> mconcat [text "(| "
               ,indent1 $ mconcat $
                 [lineBreak
                 ,describeGrammar g0
                 ,lineBreak
                 ,describeGrammar g1
                 ,lineBreak
                 ]
               ,text " |)"
               ]

  GIsoMap (Iso labels _ _) g
    -> mconcat [text "($ "
               ,text $ mconcat labels
               ,text " $)"
               ]

  GProductMap g0 g1
    -> mconcat [text "(& "
               ,describeGrammar g0
               ,text " "
               ,describeGrammar g1
               ,text " &)"
               ]

  GLabel l g
    -> mconcat [text "("
               ,text l
               ,text " "
               ,text " "
               ,text l
               ,text " )"
               ]

  GTry g0
    -> mconcat [text " (T "
               ,describeGrammar g0
               ,text " T) "
               ]

showExpectedDoc :: Expected -> Doc
showExpectedDoc = bulleted
                . flattenExpectedDoc

-- Returns alternatives
flattenExpectedDoc :: Expected -> [Doc]
flattenExpectedDoc e = case e of
  ExpectEither es0 es1
    -> flattenExpectedDoc es0 <> flattenExpectedDoc es1

  ExpectOneOf ts
    -> let oneOf = map text ts
        in if null oneOf
             then [text "_EXPECTNOTHING_"]
             else oneOf

  ExpectPredicate label mE
    -> map ((text "_PREDICATE_" <> text label) <>) $ maybe [] flattenExpectedDoc mE

  ExpectAnything
    -> [text "ANYTHING"]

  ExpectN i e
    -> [text $ "_EXACTLY_" <> (Text.pack . show $ i) <> "_"
       ,mconcat . flattenExpectedDoc $ e
       ]

  -- Show the label only
  ExpectLabel l e
    -> [text $ l <> " AKA " <> (render . mconcat . flattenExpectedDoc $ e)
       ]

-- Turn an 'Expected' into a list of each expected alternative
flattenExpected :: Expected -> [Text]
flattenExpected = map render . flattenExpectedDoc

instance Document Expected where
  document = showExpectedDoc

-- Turn an 'Expected' into a bulleted list of each unique expected alternative.
showExpected :: Expected -> Text
showExpected = Text.intercalate "\n - "
             . List.nub
             . flattenExpected

instance Document Pos where
  document (Pos t l c) = mconcat
    [text "Line:     ", int l,lineBreak
    ,text "Character:", int c,lineBreak
    ,text "Total:    ", int t,lineBreak
    ]

instance Document Cursor where
  document (Cursor prev next pos) =
    let (before,pointer,after) = point (Cursor prev next pos)
     in mconcat [rawText before
                ,lineBreak
                ,text pointer
                ,lineBreak
                ,document pos
                ,lineBreak

                -- Should be raw text!
                ,rawText after
                ]

instance Document a
      => Document (ParseResult a) where
  document p = case p of
    ParseSuccess a leftovers
      -> text "Parsed: " <> document a <> text "with leftovers" <> document leftovers

    ParseFailure failures cur0
      -> mconcat $
           [text "Parse failure at:"
           ,lineBreak

           ,indent1 $ document cur0
           ,lineBreak
           ]
           ++
           if null failures
             then mempty
             else [text "The failures backtracked from were:"
                  ,lineBreak
                  ,indent1 $ mconcat $ map (\(expected,cursor) -> mconcat [document cursor

                                                                          ,text "Expected:"
                                                                          ,document expected
                                                                          ,lineBreak
                                                                          ]
                                           )
                                           failures
                  ]

