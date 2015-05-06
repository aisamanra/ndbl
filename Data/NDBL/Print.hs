{-# LANGUAGE OverloadedStrings #-}

module Data.NDBL.Print where

import           Data.Char (isSpace)
import           Text.PrettyPrint

pretty :: [[(String, String)]] -> String
pretty = render . dNDBL

dNDBL :: [[(String, String)]] -> Doc
dNDBL = vcat . map (vcat . indentTail . map dPair)

indentTail :: [Doc] -> [Doc]
indentTail (x:xs) = x:map (nest 2) xs
indentTail xs     = xs

dPair :: (String, String) -> Doc
dPair (k,v) = text k <> equals <> dVal v

dVal :: String -> Doc
dVal t
  | any isSpace t = doubleQuotes $ text $ concatMap go t
  | otherwise     = text t
    where go '\\' = "\\\\"
          go '"'  = "\\\""
          go c    = [c]
