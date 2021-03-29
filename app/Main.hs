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

data JsonEnv = JsonEnv
    { host :: Text
    , path :: Text
    , queryLangs :: [Text]
    } deriving (Show,Generic)

instance FromJSON JsonEnv

getTranslateByKey :: Text -> ByteString -> Maybe Text
getTranslateByKey translateKey  = preview (key translateKey . _String)

readTranslateFromFile translatedKeys queryLang = do
    Prelude.appendFile ((T.unpack queryLang) ++ ".json") "{\n"
    forM_ translatedKeys printTranslatedKey
    Prelude.appendFile ((T.unpack queryLang) ++ ".json") "}"
        where
            createJsonKeyValue jKey jValue = if (lastElem == jKey) 
                then ("\t\"" ++ (T.unpack jKey) ++ "\"" ++ ": \"" ++ (T.unpack jValue) ++ "\"\n" )
                else ("\t\"" ++ (T.unpack jKey) ++ "\"" ++ ": \"" ++ (T.unpack jValue) ++ "\",\n" )
                where lastElem = Prelude.last(translatedKeys)
            printTranslatedKey translateKey = do
                jsonData <- B.readFile $ "translations_" ++ (T.unpack queryLang) ++ ".json"
                case getTranslateByKey translateKey jsonData of
                    Nothing -> Prelude.appendFile ((T.unpack queryLang) ++ ".json") $ createJsonKeyValue translateKey translateKey
                    Just translations -> Prelude.appendFile ((T.unpack queryLang) ++ ".json") $ createJsonKeyValue translateKey translations

buildRequest :: BC.ByteString
             -> BC.ByteString
             -> BC.ByteString
             -> Request
buildRequest host method path =
  setRequestMethod method
    $ setRequestHost host
    $ setRequestPath path defaultRequest


-- fetchJSON ::  IO BC.ByteString
fetchJSON host path queryLang = do
  let request = prepareRequest host path queryLang
  result <- httpBS request
  return (getResponseBody result)                    

prepareRequest :: Text -> Text -> Text -> Request
prepareRequest host path queryLang  = 
    let 
        convertedHost = encodeUtf8 (host) :: BC.ByteString
        convertedPath = encodeUtf8 (T.concat [path, queryLang]) :: BC.ByteString
    in buildRequest convertedHost "GET" convertedPath :: Request

loadTranslitions host path queryLangs translatedKeys  = do    
    forM_ queryLangs helper
    forM_ queryLangs helper'
        where
            helper queryLang = do
                translations <- fetchJSON host path queryLang
                BC.writeFile ("translations_" ++  (T.unpack queryLang) ++ ".json") translations
            helper' queryLang = readTranslateFromFile translatedKeys queryLang

loadTranslateJson translatedKeys = do
    jsonEnv <- B.readFile "jsonEnv.json"
    let jsonEnvData = decode jsonEnv :: Maybe JsonEnv
    case jsonEnvData of 
        Just (JsonEnv host path queryLangs ) -> loadTranslitions host path queryLangs translatedKeys
        Nothing -> print "error"


            
main :: IO ()
main = do
    jsonKeys <- B.readFile "keys.json"
    let dataKeys = decode jsonKeys :: Maybe TranslatedKeys
    case dataKeys of 
        Nothing -> print "error loading translation keys"
        Just (TranslatedKeys translatedKeys) -> loadTranslateJson translatedKeys