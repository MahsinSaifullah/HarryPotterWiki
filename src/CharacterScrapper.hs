-- |Module that performs http requests as well as parsing returned html and creates a character record.
module CharacterScrapper ( newCharacter ) where

import Network.HTTP.Simple -- http-conduit
import qualified Data.ByteString.Lazy.Char8 as B --bytestring
import Control.Monad
import Text.HTML.TagSoup --tagsoup

data Character = Character {

  name::String,
  gender::String,
  bloodStatus::String,
  dob::String,
  house::String,
  patronus::String

} deriving (Show)

urlBuilder name = "https://harrypotter.fandom.com/wiki/" ++ name

getTags url = do
  req <- parseRequest $ url
  res <- httpLBS req
  let tags = parseTags $ B.unpack (getResponseBody res)
  return $ tags

newCharacter name = do
  fname <- getFullName name
  gender <- getGender name
  bloodStatus <- getBloodStatus name
  dob <- getDOB name
  house <- getHouse name
  patronus <- getPatronus name
  let newCharacter = Character fname gender bloodStatus dob house patronus
  return  newCharacter


getFullName name = do
  tags <- getTags $ urlBuilder name
  let data' = head $ sections (~== "<h2 data-source=name>") tags
  let name = innerText data'
  return $ (words name) !! 0 ++ " " ++ (words name) !! 1 ++ " " ++ if (words name) !! 2 == "Biographical" then "" else (words name) !! 2


getGender name = do
  tags <- getTags $ urlBuilder name
  let data' = head $ sections (~== "<div data-source=gender>") tags
  let gender = innerText data'
  return $ takeWhile (/='[') $ (words gender) !! 1


getBloodStatus name = do
  tags <- getTags $ urlBuilder name
  let data' = head $ sections (~== "<div data-source=blood>") tags
  let status = innerText data'
  return $ takeWhile(/='[') $ (words status) !! 2


getDOB name = do
  tags <- getTags $ urlBuilder name
  let data' = head $ sections (~== "<div data-source=born>") tags
  let dob = innerText data'
  return $ takeWhile (/='[') $ (words dob) !! 1 ++ " " ++  (words dob) !! 2


getHouse name = do
  tags <- getTags $ urlBuilder name
  let data' = head $ sections (~== "<div data-source=house>") tags
  let house = innerText data'
  return $ takeWhile(/='[') $ (words house) !! 1


getPatronus name = do
  tags <- getTags $ urlBuilder name
  let data' = head $ sections (~== "<div data-source=patronus>") tags
  let patronus = innerText data'
  return $ takeWhile(/='[') $ (words patronus) !! 1
