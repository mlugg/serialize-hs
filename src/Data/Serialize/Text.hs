module Data.Serialize.Text where

import Data.Functor.Contravariant
import Data.Serialize
import Data.Text(Text)
import qualified Data.Text as T

type S = Serialize Text

lit :: Text -> S ()
lit t = Serialize $ const t

text :: S Text
text = Serialize id

char :: S Char
char = Serialize $ T.singleton

string :: S String
string = Serialize $ T.pack

serShow :: (Show a) => S a
serShow = show >$< string
