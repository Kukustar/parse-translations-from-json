{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B

import GHC.Generics

import Control.Monad
import Control.Lens ( preview )
import Data.Aeson.Lens ( key, _String)

import qualified Data.ByteString.Char8  as BS

data TranslatedKeys = TranslatedKeys
    { translatedKeys :: [T.Text]
    } deriving (Show,Generic)

instance FromJSON TranslatedKeys

printKeys :: Maybe [T.Text] -> IO ()
printKeys Nothing = print "error loading translated keys"
printKeys (Just translatedKeys) = forM_ translatedKeys (readTranslateFromFile)

getTranslateByKey :: Text -> ByteString -> Maybe Text
getTranslateByKey translateKey  = preview (key translateKey .  _String)

readTranslateFromFile :: Text -> IO ()
readTranslateFromFile key = do
    jsonData <- B.readFile "data.json"
    case getTranslateByKey key jsonData of
        Nothing -> print "error loading translations"
        Just translations -> print translations

main :: IO ()
main = do
    jsonKeys <- B.readFile "keys.json"
    let dataKeys = decode jsonKeys :: Maybe TranslatedKeys
    let keys = translatedKeys <$> dataKeys
    printKeys keys