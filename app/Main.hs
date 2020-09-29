{-# LANGUAGE OverloadedStrings #-}

module Main where
import ASCII
import Data.Aeson
import Data.Text as T
import Data.Text.Encoding
import Data.ByteString.Lazy as B

import Text.Printf

import GHC.Generics

import Control.Monad
import Control.Lens ( preview )
import Data.Aeson.Lens ( key, _String)

data TranslatedKeys = TranslatedKeys
    { translatedKeys :: [T.Text]
    } deriving (Show,Generic)

instance FromJSON TranslatedKeys

printKeys :: Maybe [T.Text] -> IO ()
printKeys Nothing = print "error loading translated keys"
printKeys (Just translatedKeys) = readTranslateFromFile translatedKeys

getTranslateByKey :: Text -> ByteString -> Maybe Text
getTranslateByKey translateKey  = preview (key translateKey .  _String)

readTranslateFromFile translatedKeys = do
    forM_ translatedKeys printTranslatedKey
        where
            printTranslatedKey translateKey = do
                jsonData <- B.readFile "data.json" 
                case getTranslateByKey translateKey jsonData of
                    Nothing -> Prelude.appendFile "test.json" $ ("errro " ++ (T.unpack translateKey) ++ "\n")
                    Just translations -> Prelude.appendFile "test.json" $ (T.unpack translations ++ "\n")

main :: IO ()
main = do
    jsonKeys <- B.readFile "keys.json"
    let dataKeys = decode jsonKeys :: Maybe TranslatedKeys
    let keys = translatedKeys <$> dataKeys
    printKeys keys