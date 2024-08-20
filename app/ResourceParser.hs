module ResourceParser where

import           Control.Applicative        ((<|>))
import           Data.Aeson                 (FromJSON, Object, decode)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Text                  (Text, pack)
import           Text.Parsec                (alphaNum, anyChar, char, choice,
                                             digit, many1, manyTill, oneOf,
                                             space, try)
import           Text.Parsec.Combinator     (eof)
import           Text.Parsec.String         (Parser)

data Spec = Spec {
      name             :: !Text
    , resourceProvider :: !Text
    , resourceType     :: !Text
    , apiVersion       :: !Text
    , payload          :: Maybe Object
} deriving Show

parseSpec ::  Parser Spec
parseSpec = Spec <$> (char '>' *> many1 space *> trailSpace parseName) <*> trailSpace parseResourceProvider <*> trailSpace parseType <*> trailSpace parseApiVersion <*> parsePayload
    where
        trailSpace :: Parser b -> Parser b
        trailSpace p = p <* many1 space
        parseName :: Parser Text
        parseName = pack <$> many1 (choice (map try [alphaNum, oneOf ['-', '_']]))
        parseResourceProvider :: Parser Text
        parseResourceProvider = pack <$> do
            first <- many1 alphaNum
            dot <- char '.'
            second <- many1 alphaNum
            return $ first ++ (dot:second)
        parseType  :: Parser Text
        parseType = pack <$> many1 alphaNum
        parseApiVersion  :: Parser Text
        parseApiVersion = pack <$> do
            year <- many1 digit <* char '-'
            month <- many1 digit <* char '-'
            day <- many1 digit
            return $ year ++ ('-':month) ++ ('-':day)
        parsePayload  :: Parser (Maybe Object)
        parsePayload = do
            text <- BS.pack <$> manyTill anyChar ((char '>' *> many1 space >> return ()) <|> eof)
            return $ decode text
