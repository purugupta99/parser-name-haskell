{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text, pack)
import Data.Void
import Control.Applicative hiding (many, some)
import Control.Monad.IO.Class
import Text.Megaparsec
import Text.Megaparsec.Char as M
import Text.Megaparsec.Char.Lexer as L

inp = ["Simon King", "Simon King-Kong", "Simon-Sequeira Vinson King-Lord"]

data Name = Name {
      fName :: Text
    , mName :: Maybe Text
    , lName :: Text
} deriving (Show)

data NameBlock = NameBlock {
    nameB :: Text
  , restName :: NameBlock
} | Empty deriving (Show)

sKing = NameBlock {
    nameB = "Simon"
    , restName = NameBlock {
        nameB = "King"
        , restName = Empty
    }
}

parseNameBlock :: Parser NameBlock
parseNameBlock = do
    x <- parseString <* spaceConsumer
    if null x
        then pure Empty
        else do
            y <- parseNameBlock
            pure $ NameBlock {
                    nameB = pack x
                    , restName = y
                }


tmpParsed = Name {
      fName = "Simon"
    , mName = Nothing
    , lName = "King"
}

type Parser a = Parsec Void Text a

spaceConsumer :: Parser ()
spaceConsumer = M.space

parseString :: Parser String
parseString = many (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['-']) :: Parser Char)

-- parseTest (satisfy (oneOf ['a'..'z']) :: Parser Char) "a"

parseName :: Parser Name
parseName = do
    -- fN <- parseString <* spaceConsumer
    -- mN <- parseString <* spaceConsumer
    -- lN <- parseString 

    fN <- parseString <* spaceConsumer
    mN <- parseString <* spaceConsumer
    lN <- parseString

    let
        temp = parseMaybe spaceConsumer

    pure (Name {
        fName = pack fN
        , mName = if null lN then Nothing else Just $ pack mN
        , lName = if null lN then pack mN else pack lN
    })

main :: IO ()
main = do
    let
        out = fmap (parseTest parseName) inp
        out2 = fmap (parseTest parseNameBlock) inp
    sequence_ out
    sequence_ out2
