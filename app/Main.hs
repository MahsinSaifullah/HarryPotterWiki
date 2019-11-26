module Main where
import CharacterScrapper
import MovieListScrapper



main :: IO ()
main = do
    newChar <- newCharacter "Harry_Potter"
    movieList <- getMovieList "http://harrypotter.fandom.com/wiki/Harry_Potter_(film_series)"
    print $ newChar
    print $ movieList
