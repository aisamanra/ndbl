{-# LANGUAGE OverloadedStrings #-}

module Data.NDBL.Parse (pNDBL) where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Char (isSpace)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Word (Word8)
import           Prelude hiding (takeWhile)

type Pair = (Text, Text)



isSep :: Char -> Bool
isSep ' '  = True
isSep '\t' = True
isSep '\r' = True
isSep '\n' = True
isSep '='  = True
isSep _    = False

hSpace :: Parser ()
hSpace = skipWhile isHorizontalSpace

pString :: Parser Text
pString = string "\"" *> (T.pack <$> sBody)
  where sBody = do
          c <- anyChar
          case c of
            '\\' -> (:) <$> anyChar <*> sBody
            '"'  -> return []
            _    -> (c:) <$> sBody

pWord :: Parser Text
pWord = takeWhile (not . isSep)

pWord1 :: Parser Text
pWord1 = takeWhile1 (not . isSep)

pPair :: Parser Pair
pPair = (,) <$> pWord1 <*. "=" <*> (pString <|> pWord)

data NewBlock
  = StartNewBlock
  | ContinueBlock
  | StopParsing
    deriving (Eq,Show)

sNextMSet :: Parser NewBlock
sNextMSet = do
  hSpace
  n <- peekChar
  go n
    where go (Just '\n') = do
            _ <- anyChar
            n <- peekChar
            go' n
          go (Just '#') = do
            skipWhile (not . (== '\n'))
            sNextMSet
          go (Just _) = return ContinueBlock
          go Nothing  = return StopParsing
          go' (Just '\n') = do
            _ <- anyChar
            n <- peekChar
            go' n
          go' (Just '#') = do
            skipWhile (not . (== '\n'))
            n <- peekChar
            go' n
          go' (Just c) | isSpace c = sNextMSet
                       | otherwise = return StartNewBlock
          go' Nothing = return StopParsing

pBlock :: Parser (NewBlock, [Pair])
pBlock = do
  p <- pPair
  b <- sNextMSet
  case b of
    StartNewBlock -> return (StartNewBlock, [p])
    StopParsing   -> return (StopParsing,   [p])
    ContinueBlock -> do
      (b, ps) <- pBlock
      return (b, p:ps)

pBlocks :: Parser [[Pair]]
pBlocks = do result <- pBlock
             go result
  where go (StartNewBlock, ps) = (ps:) <$> pBlocks
        go (StopParsing, ps)   = return [ps]
        go _                   = error "unreachable"

pNDBL :: Text -> Either String [[Pair]]
pNDBL t = parseOnly pBlocks t
