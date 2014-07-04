-- | NDBL is a simple configuration format designed after the @ndb(6)@ set of
-- utilities used in the Plan 9 operating system. An NDBL file is a sequence
-- of groups, where each group is a multiset of key-value pairs. This will
-- cover the basics of NDBL; for a more in-depth explanation, consult
-- <https://github.com/aisamanra/ndbl the github page>.
--
-- Grouping in NDBL is done by
-- indentation: a new group is started by listing a key-value pair without
-- indentation, while indentation (of any amount) means that the key-value
-- pairs on that line belong to the previous group. A key-value pair consists
-- of any non-space identifiers, followed by an '=', followed by another
-- sequence of non-space identifiers. For example, the following parses to
-- @[[(\"user\",\"alice\"),(\"city\",\"paris\")]]@:
--
-- @
-- user=alice
--   city=paris
-- @
--
-- Whereas this one parses to @[[(\"user\",\"alice\")],[(\"city\",\"paris\")]]@:
--
-- @
-- user=alice
-- city=paris
-- @
--
-- Empty values are allowed, but not empty keys, so the following is valid and
-- parses as @[[(\"database\",\"\"),(\"file\",\"file1.txt\"),(\"file\",\"file2.txt\")]]@
--
-- @
-- database=
--   file=file1.txt
--   file=file2.txt
-- @
--
-- Be aware that NDBL guarantees that
--
-- @
-- fromJust . decode . encode == id
-- @
--
-- but does NOT guarantee that
--
-- @
-- encode . fromJust . decode == id
-- @

module Data.NDBL ( decode
                 , encode
                 ) where

import Data.Text (Text)

import Data.NDBL.Parse
import Data.NDBL.Print

-- | Decode an NDBL document to a list of lists of key-value pairs.
decode :: Text -> Maybe [[(Text, Text)]]
decode t = case pNDBL t of
  Right r -> Just r
  Left _  -> Nothing

-- | Decode an NDBL document to a list of lists of key-value pairs,
--   supplying the parse error from "attoparsec" if decoding fails.
decodeEither :: Text -> Either String [[(Text, Text)]]
decodeEither = pNDBL

-- | Encode an NDBL document to its 'Text' representation.
encode :: [[(Text, Text)]] -> Text
encode = pretty
