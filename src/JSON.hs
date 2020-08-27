{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module JSON where

import Data.Serialize
import Data.Serialize.Combinators
import Data.Text(Text)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Serialize.Text as S
import Numeric(showHex)
import Data.Char

data JSONElement
  = JSONObject [(Text, JSONElement)]
  | JSONArray [JSONElement]
  | JSONString Text
  | JSONBool Bool
  | JSONNum (Either Integer Double)
  | JSONNull

serialize :: Serialize Text JSONElement
serialize = Serialize $ \case
  JSONObject x -> runSerializer serObject x
  JSONArray  x -> runSerializer serArray  x
  JSONString x -> runSerializer serString x
  JSONBool   x -> runSerializer serBool   x
  JSONNum    x -> runSerializer serNum    x
  JSONNull     -> runSerializer serNull   ()
  where
    serObject = between (S.lit "{") (S.lit "}") $
      (quoted >*< (S.lit ": " *< serialize)) `sepBy` S.lit ", "

    serArray = between (S.lit "[") (S.lit "]") $
      serialize `sepBy` S.lit ", "

    serString = quoted

    serBool = (\x -> if x then "true" else "false") >$< S.text

    serNum = S.serShow >|< S.serShow

    serNull = S.lit "null"

    quoted = between (S.lit "\"") (S.lit "\"") $
      escape >$< S.text

    escape = T.concatMap escapeChar
    escapeChar c = fromMaybe (if isPrint c then T.singleton c else c') $ lookup c escs
      where c' = "\\u" <> T.justifyRight 4 '0' (T.pack $ showHex (fromEnum c) "")
    escs =
      [ ('"', "\\\"")
      , ('\\', "\\\\")
      , ('\b', "\\b")
      , ('\f', "\\f")
      , ('\n', "\\n")
      , ('\r', "\\r")
      , ('\t', "\\t") ]
