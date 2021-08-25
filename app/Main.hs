{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text, pack, unpack)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char as M

{-
> an uppercase (resp. lowercase) letter is a character in the 26-character 
  uppercase (resp. lowercase) Latin alphabet used to write English

> a name unit is an uppercase letter followed by zero or more lowercase 
  letters

> a name is one or more name units separated by hyphens

> a full name consists of names separated by spaces: a first name, then 
  an optional middle name, and finally a last name
-}

inp = ["Simon King", "Simon King-Kong", "Simon-Sequeira Vinson King-Lord"]

newtype NameUnit = NameUnit Text
newtype NameB = NameB [NameUnit]

instance Show NameB where
    show (NameB []) = []
    show (NameB [a]) = show a
    show (NameB (a:lst)) = show a ++ "-" ++ show (NameB lst)

instance Show NameUnit where
    show (NameUnit a) = unpack a

instance Show NameBlock where
    show Empty = ""
    show name = show (nameB name) ++  " " ++ show (restName name)

data NameBlock = NameBlock {
    nameB :: NameB
  , restName :: NameBlock
} | Empty

data Name = Name {
      fName :: NameB
    , mName :: NameB
    , lName :: NameB
} deriving (Show)

parseNameBlock :: Parser NameBlock
parseNameBlock = do
    x <- parseNameB <* spaceConsumer
    if checkNull x
        then pure Empty
        else do
            y <- parseNameBlock
            pure $ NameBlock {
                    nameB = x
                    , restName = y
                }

type Parser a = Parsec Void Text a

spaceConsumer :: Parser ()
spaceConsumer = M.space

parseNameUnit :: Parser NameUnit
parseNameUnit = do
    -- first char is capital alphabet
    c1 <- M.upperChar
    rest <- many M.lowerChar
    return $ NameUnit $ pack (c1 : rest)

parseChar :: Char -> Parser Text
parseChar c = do
    c' <- M.char c
    return $ pack [c']

parseNameB :: Parser NameB
parseNameB = do
    -- a <- many (parseChar '-' >> parseNameUnit)
    a <- parseNameUnit `sepBy` parseChar '-'
    return $ NameB a

checkNull :: NameB -> Bool
checkNull (NameB a) = null a

parseName :: Parser Name
parseName = do
    fN <- parseNameB <* spaceConsumer
    mN <- parseNameB <* spaceConsumer
    lN <- parseNameB

    pure (Name {
        fName = fN
        , mName = if checkNull lN then NameB [] else mN
        , lName = if checkNull lN then mN else lN
    })

main :: IO ()
main = do
    let
        out = fmap (parseTest parseName) inp
        out2 = fmap (parseTest parseNameBlock) inp
    sequence_ out
    sequence_ out2