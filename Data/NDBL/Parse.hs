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

-- | Skips any horizontal (non-new-line) spaces
hSpace :: Parser ()
hSpace = skipWhile isHorizontalSpace

-- | Parses a quotedd string
pQString :: Parser Text
pQString = string "\"" *> (T.pack <$> sBody)
  where sBody = do
          c <- anyChar
          case c of
            '\\' -> (:) <$> anyChar <*> sBody
            '"'  -> return []
            _    -> (c:) <$> sBody

-- | Parse a word of any length
pWord :: Parser Text
pWord = takeWhile (not . isSep)

-- | Parse a word of at least length one
pWord1 :: Parser Text
pWord1 = takeWhile1 (not . isSep)

-- | Parse a key-value pair
pPair :: Parser Pair
pPair = (,) <$> pWord1 <*. "=" <*> (pQString <|> pWord)

-- | Directions passed back from the skipping parsers---whether
--   to continue parsing the current block, start a new block,
--   or stop entirely.
data NewBlock
  = StartNewBlock
  | ContinueBlock
  | StopParsing
    deriving (Eq,Show)

comment :: Parser ()
comment = char '#' >> skipWhile (not . (== '\n'))

-- | Skips to the next pair (or end of input) and returns whether to parse
--   the next pair as part of the same group, the next group, or whether
--   it's reached the end.
sSkipToNext :: Bool -> Parser NewBlock
sSkipToNext False = hSpace >> peekChar >>= go
    where go (Just '\n') = anyChar >> sSkipToNext True
          go (Just '\r') = anyChar >> sSkipToNext True
          go (Just '#')  = comment >> sSkipToNext False
          go (Just _)    = return ContinueBlock
          go Nothing     = return StopParsing
sSkipToNext True = peekChar >>= go
  where go (Just c) | isSpace c = anyChar >> sSkipToNext False
                    | otherwise = return StartNewBlock
        go Nothing  = return StopParsing

pBlock :: Parser (NewBlock, [Pair])
pBlock = do
  p <- pPair
  b <- sSkipToNext False
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
