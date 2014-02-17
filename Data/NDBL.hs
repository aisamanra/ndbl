-- | NDBL is a simple configuration format designed after the @ndb(6)@ set of
-- utilities used in the Plan 9 operating system. An NDBL file is a sequence
-- of groups, where each group is a multiset of key-value pairs. This will
-- cover the basics of NDBL; for a more in-depth explanation, consult
-- [https://github.com/aisamanra/ndbl](the github page).
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

module Data.NDBL ( -- * Convenience Types
                   NDBL
                 , Pair
                   -- * MultiMap Representation
                   -- $mm
                 , decode
                 , encode
                   -- * List-Of-List Representation
                   -- $ll
                 , decodeList
                 , encodeList
                   -- * Flat List Representation
                   -- $fl
                 , decodeFlat
                 , encodeFlat
                 ) where

import           Data.MultiMap (MultiMap)
import qualified Data.MultiMap as M
import           Data.Text (Text)

import Data.NDBL.Parse
import Data.NDBL.Print

type NDBL  = [MultiMap Text Text]
type Pair  = (Text, Text)

-- $mm
-- The most convenient way of parsing an NDBL file is as a
-- multimap. In this case, the set of groups is given in-order
-- as a list, and each individual group is represented as a
-- multimap with text keys and values.
--
-- This does mean that the empty string is a possible value for the multimap,
-- although all the keys will be at least one character long.

decode :: Text -> Maybe NDBL
decode t = case pNDBL t of
  Right r -> Just (map M.fromList r)
  Left _  -> Nothing

encode :: NDBL -> Text
encode = pretty . map M.toList

-- $ll
-- Not every application wants to bring in a dependency on the
-- multimap package, so functions that deal with lists of lists
-- of tuples are also provided.

decodeList :: Text -> Maybe [[Pair]]
decodeList t = case pNDBL t of
  Right r -> Just r
  Left _  -> Nothing

encodeList :: [[Pair]] -> Text
encodeList = pretty

-- $fl
-- Often a config file doesn't need grouping, so these are offered
-- for pure utility.

decodeFlat :: Text -> Maybe [Pair]
decodeFlat t = case pNDBL t of
  Right r -> Just $ concat r
  Left _  -> Nothing

encodeFlat :: [Pair] -> Text
encodeFlat = pretty . map (:[])
