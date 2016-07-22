module Util
  (putColor
  ,putGreen
  ,putRed
  ,putYellow
  ,putBlue
  ,putCyan


  )
  where

-- Color a line of text
putColor :: Int -> String -> IO ()
putColor i s = putStr ("\x1b["++show i++"m")
            >> putStrLn s
            >> putStr "\x1b[0m"

-- Color a line of text. Newline
putGreen  = putColor 32
putRed    = putColor 31
putYellow = putColor 33
putBlue   = putColor 34
putCyan   = putColor 36


