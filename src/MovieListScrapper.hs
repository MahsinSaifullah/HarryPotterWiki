module MovieListScrapper ( getMovieList ) where

import Network.HTTP.Simple -- http-conduit
import qualified Data.ByteString.Lazy.Char8 as B --bytestring
import Control.Monad
import Text.HTML.TagSoup --tagsoup

data MovieList = HarryPotterMovies {

  movie1::String,
  movie2::String,
  movie3::String,
  movie4::String,
  movie5::String,
  movie6::String,
  movie7::String,
  movie8::String

} deriving (Show)

url = "http://harrypotter.fandom.com/wiki/Harry_Potter_(film_series)"

getTags url = do
  req <- parseRequest $ url
  res <- httpLBS req
  let tags = parseTags $ B.unpack (getResponseBody res)
  return $ tags

getMovieList url = do
  tags <- getTags $ url
  let data' = head $ sections (~== "<span id=Films>") tags
  let movie = innerText data'
  let movie1 = unwords $ drop 7 . take 13 $ words movie
  let movie2 = unwords $ drop 25 . take 32 $ words movie
  let movie3 = unwords $ drop 44 . take 51 $ words movie
  let movie4 = unwords $ drop 63 . take 70 $ words movie
  let movie5 = unwords $ drop 78 . take 86 $ words movie
  let movie6 = unwords $ drop 96 . take 102 $ words movie
  let movie7 = unwords $ drop 112 . take 120 $ words movie
  let movie8 = unwords $ drop 132 . take 140 $ words movie
  let movieList = HarryPotterMovies movie1 movie2 movie3 movie4 movie5 movie6 movie7 movie8
  return movieList
