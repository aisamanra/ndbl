{-# LANGUAGE OverloadedStrings #-}

module Data.NDBL.Print where

import           Data.Char (isSpace)
import           Data.Text (Text,unpack,pack,concatMap,singleton,any)
import           Text.PrettyPrint
import           Prelude hiding (concatMap,any)

pretty :: [[(Text, Text)]] -> Text
pretty = pack . render . dNDBL

dNDBL :: [[(Text, Text)]] -> Doc
dNDBL = vcat . map (vcat . indentTail . map dPair)

indentTail :: [Doc] -> [Doc]
indentTail (x:xs) = x:map (nest 2) xs
indentTail xs     = xs

dPair :: (Text, Text) -> Doc
dPair (k,v) = text (unpack k) <> equals <> dVal v

dVal :: Text -> Doc
dVal t
  | any isSpace t = doubleQuotes $ text $ unpack $ concatMap go t
  | otherwise     = text (unpack t)
    where go '\\' = "\\\\"
          go '"'  = "\\\""
          go c    = singleton c
