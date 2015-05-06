{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Data.NDBL.Parse (Document, Group, Pair, pNDBL) where

type Document = [Group]
type Group    = [Pair]
type Pair     = (String,String)

type Result a = Either String (String, a)
type Parse a = String -> Result a

throw = Left

over :: (a -> b) -> Result a -> Result b
over _ (Left err)     = Left err
over f (Right (s, x)) = Right (s, f x)

bind :: Result a -> ((String, a) -> Result b) -> Result b
bind (Left err) _ = Left err
bind (Right a)  f = f a

isSep :: Char -> Bool
isSep c = c `elem` " \t\r\n="

pQString :: Parse String
pQString = go
  where go ('\\':x:xs) = (x:) `over` go xs
        go ('"':xs) = return (xs, "")
        go (x:xs) = (x:) `over` go xs

pWord :: Parse String
pWord s@(x:xs)
  | not (isSep x) = (x:) `over` pWord xs
pWord s = return (s, "")

pWord1 :: Parse String
pWord1 (x:xs)
  | not (isSep x) = (x:) `over` pWord xs
pWord1 s = throw $ "Expected word; found " ++ show s


pPair :: Parse (String, String)
pPair s = bind (pWord1 s) $ \case
  ('=':'"':s', r) -> (r,) `over` pQString s'
  ('=':s',     r) -> (r,) `over` pWord s'
  _               -> throw "Expected '=' after pair name"

isHSpace :: Char -> Bool
isHSpace c = c == ' ' || c == '\t'

isVSpace :: Char -> Bool
isVSpace c = c == '\n' || c == '\r'

pSkip :: Parse Bool
pSkip (y:[]) = return ("", False)
pSkip (y:s@(x:xs))
  | isVSpace y && isHSpace x = pSkip xs
  | isVSpace y               = return (s, False)
pSkip s@(x:xs)
  | isHSpace x = pSkip xs
  | otherwise  = return (s, True)


pGroup :: Parse [(String, String)]
pGroup s = bind (pPair s) $ \case
  (s', p) -> bind (pSkip s') $ \case
    (s'', True)  -> (p:) `over` pGroup s''
    (s'', False) -> return (s'', [p])

pDocument :: Parse Document
pDocument s = bind (pGroup s) $ \case
  ("", g) -> return ("", [g])
  (xs, g) -> (g:) `over` pDocument xs

pNDBL :: String -> Either String Document
pNDBL s = case pDocument s of
  Right (_, d) -> Right d
  Left err     -> Left err
