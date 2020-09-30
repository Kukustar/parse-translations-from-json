{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Text as T
import Data.Text.Encoding
import Data.ByteString.Lazy as B

import Text.Printf

import GHC.Generics

import Control.Monad
import Control.Lens ( preview )
import Data.Aeson.Lens ( key, _String)

-- network 
import qualified Data.ByteString.Char8  as BC
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status

data TranslatedKeys = TranslatedKeys
    { translatedKeys :: [T.Text]
    } deriving (Show,Generic)

instance FromJSON TranslatedKeys

printKeys :: Maybe [T.Text] -> IO ()
printKeys Nothing = print "error loading translated keys"
printKeys (Just translatedKeys) = readTranslateFromFile translatedKeys

getTranslateByKey :: Text -> ByteString -> Maybe Text
getTranslateByKey translateKey  = preview (key translateKey . _String)

readTranslateFromFile translatedKeys = do
    Prelude.appendFile "en.json" "{\n"
    forM_ translatedKeys printTranslatedKey
    Prelude.appendFile "en.json" "}"
        where
            -- todo убрать запятые из конца файла
            createJsonKeyValue jKey jValue = ("\t\"" ++ (T.unpack jKey) ++ "\"" ++ ": \"" ++ (T.unpack jValue) ++ "\",\n" )
            printTranslatedKey translateKey = do
                jsonData <- B.readFile "translations_en.json"
                case getTranslateByKey translateKey jsonData of
                    Nothing -> Prelude.appendFile "en.json" $ createJsonKeyValue translateKey translateKey
                    Just translations -> Prelude.appendFile "en.json" $ createJsonKeyValue translateKey translations

buildRequest :: BC.ByteString
             -> BC.ByteString
             -> BC.ByteString
             -> Request
buildRequest host method path =
  setRequestMethod method
    $ setRequestHost host
    $ setRequestPath path defaultRequest


fetchJSON ::  IO BC.ByteString
fetchJSON = do
  let host = "social-mt5.tifia.com" :: BC.ByteString
  let path = "/en/api/translate/get-translations?lang=en"
--   let queryString = "lang=en" :: Q
  let request = buildRequest host "GET" path  :: Request
  result <- httpBS request
  return (getResponseBody result)                    

loadTranslitions :: IO ()
loadTranslitions = do
    translations <- fetchJSON 
    BC.writeFile "translations_en.json" translations
            
main :: IO ()
main = do
    -- let url = "/en/api/translate/get-translations?lang=en" :: BC.ByteString
    loadTranslitions 
    jsonKeys <- B.readFile "keys.json"
    let dataKeys = decode jsonKeys :: Maybe TranslatedKeys
    let keys = translatedKeys <$> dataKeys
    printKeys keys