{-|

Module     : Data.Serialize.Text
Copyright  : © Matthew Lugg, 2020
License    : Unlicense
Maintainer : mlugg@mlugg.co.uk
Stability  : experimental

This module defines some simple serializers for use with 'Text'.

-}

{-# LANGUAGE Safe #-}

module Data.Serialize.Text where

import Data.Functor.Contravariant
import Data.Serialize
import Data.Text(Text)
import qualified Data.Text as T

-- | 'lit' creates a serializer which ignores its input and simply
-- outputs its argument @t@.
lit :: Text -> Serialize Text ()
lit t = Serialize $ const t

-- | 'text' simply returns the 'Text' it is given.
text :: Serialize Text Text
text = Serialize id

-- | 'char' creates a singleton 'Text' from a 'Char'.
char :: Serialize Text Char
char = Serialize $ T.singleton

-- | 'string' uses 'T.pack' to convert the input 'String' into a 'Text'.
string :: Serialize Text String
string = Serialize $ T.pack

-- | 'serShow' serializes any object with a 'Show' instance by calling
-- 'show' and then converting the 'String' into 'Text'.
serShow :: (Show a) => Serialize Text a
serShow = show >$< string
