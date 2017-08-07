{- 
    Queries the dog.ceo API. Provides three functions:
    - getRandomDog -- get a random photo of a dog (of any breed)
    - getRandomDogByBreed -- get a random photo of a dog of a specified breed
    - getBreedList -- get the list of breeds

    Author: Gabriel Fishman
-}
{-# LANGUAGE OverloadedStrings #-}

module DogRetriever
    ( getRandomDog,
      getRandomDogByBreed,
      getBreedList
    ) where

import Control.Lens
import Data.Aeson.Lens (_Array, _String, key)
import Data.Aeson.Text
import Data.Text
import qualified Data.Text.Lazy as L
import Data.Vector (toList)
import Network.Wreq

getRandomDog :: IO Text
getRandomDog = queryAPI "https://dog.ceo/api/breeds/image/random" 

getRandomDogByBreed :: String -> IO Text
getRandomDogByBreed breed = queryAPI $ "https://dog.ceo/api/breed/" ++ breed ++ "/images/random"

getBreedList :: IO Text
getBreedList = do
    r <- get "https://dog.ceo/api/breeds/list"
    let message = toList $ r ^. responseBody . key "message" . _Array
    let breedList = L.intercalate "\n" (Prelude.foldr (\x acc -> (removeQuotes . encodeToLazyText) x : acc) [] message)
    return $ L.toStrict breedList
    

queryAPI :: String -> IO Text
queryAPI url = do
    r <- get url
    let message = r ^. responseBody . key "message" . _String
    return message

removeQuotes :: L.Text -> L.Text
removeQuotes = L.filter (/= '"')
