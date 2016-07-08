module PL.Printer.Debug
  (trace
  ,traceId

  ,traceIndent
  ,traceStep
  )
  where

import PL.Printer

import qualified Debug.Trace as D
import qualified Data.Text as Text

-- Debug.Trace from base, but takes a Document d instead of a String
trace :: Document d => d -> a -> a
trace = D.trace . Text.unpack . renderDocument

-- Debug.TraceId from base, but takes a Document d instead of a String
traceId :: Document d => d -> d
traceId d = trace d d


-- Trace a Document d, indented a given number of spaces.
traceIndent :: Document d => Int -> d -> a -> a
traceIndent i d = trace (indent i . document $ d)

-- Trace a Document d. Formatted between braces and indented by 4 spaces.
-- Similar to a step in a mathematical derivation.
traceStep :: Document d => d -> a -> a
traceStep d = traceIndent 4 (between (char '{',char '}') . document $ d)
