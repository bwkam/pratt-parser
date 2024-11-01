module Main where

import Control.Applicative
import Data.Char

main :: IO ()
main = return ()

newtype Parser a = Parser {parse :: String -> Either String (a, String)}

runParser :: Parser a -> String -> Either String a
runParser (Parser p) s = fmap fst (p s)

instance Functor Parser where
    fmap f (Parser p) = Parser go
      where
        go st = case p st of
            Left e -> Left e
            Right (res, str) -> Right (f res, str)

instance Applicative Parser where
    pure x = Parser $ \st -> Right (x, st)
    (Parser p) <*> (Parser p') = Parser go
      where
        go st = case p st of
            Left e -> Left e
            Right (f, st') -> case p' st' of
                Left e' -> Left e'
                Right (res, st'') -> Right (f res, st'')

instance Alternative Parser where
    empty = Parser $ \_ -> Left "nothing in here"
    (Parser p) <|> (Parser p') = Parser go
      where
        go st = case p st of
            Left _ -> p' st
            Right x -> Right x

instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser go
      where
        go st = case p st of
            Left e -> Left e
            Right (res, st') -> parse (f res) st'

fail m = Parser $ \_ -> Left m

any :: Parser Char
any = Parser go
  where
    go [] = Left "any: end of file"
    go (x : xs) = Right (x, xs)

peek :: Parser Char
peek = Parser go
  where
    go [] = Left "nothing to look at"
    go (x : xs) = Right (x, x : xs)

lbp :: Char -> Int
lbp x
    | (x == '+') = 2
    | (x == '*') = 3
    | (x == '$') = -1
    | isDigit x = 0
    | otherwise = 0

nud :: Char -> Parser String
nud x = return [x]

led :: String -> Char -> Parser String
led l t =
    expr (lbp t)
        >>= \x -> return $ "(" <> [t] <> " " <> l <> " " <> x <> ")"

expr :: Int -> Parser String
expr rbp = do
    x <- Main.any
    t <- peek

    l <- go (lbp t > rbp) [x]
    return l
  where
    go :: Bool -> String -> Parser String
    go False x = return x
    go True x = do
        t <- Main.any
        l <- led x t

        t' <- peek
        l' <-
            if lbp t' > rbp
                then do
                    l' <- go True l
                    return l'
                else return l
        return l'
