{-
    Mattermost bot that queries the dogs.ceo API and returns random pictures of dogs.

    Author: Gabriel Fishman
-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe
import DogRetriever
import Web.Scotty 
import qualified Data.Text as T
import qualified Data.Text.Lazy as L

data QueryResponse = QueryResponse { textResponse :: IO T.Text
                                   , isEphemeral :: Bool } 

main :: IO ()
main = scotty 3300 $ do
    post "/dog" $ do 
        p <- params
        let textParam = lookup "text" p
        let queryResponse = handleQuery textParam
        liftedResponse <- liftIO $ textResponse queryResponse
        Web.Scotty.json $ getJsonResponse (isEphemeral queryResponse) liftedResponse

handleQuery :: Maybe L.Text -> QueryResponse
handleQuery Nothing = QueryResponse getRandomDog False
handleQuery (Just cmd)
    | null input = QueryResponse getRandomDog False
    | head input == "breeds" = QueryResponse getBreedList True
    | head input == "help" = QueryResponse getHelp True
    | otherwise = QueryResponse (getRandomDogByBreed $ head input) False
    where
        input = (words . L.unpack) cmd

getHelp :: IO T.Text
getHelp = return "Usage: \n\
    \- /dog -- shows a random image of a dog\n\
    \- /dog <breed> -- shows a random image of <breed>\n\
    \- /dog breeds -- shows a list of breeds (will show only to you, not the whole channel)\n\
    \- /dog help -- shows this help message" 

-- ephemeral messages are only readable by the sender, in_channel messages are readable by everyone in the channel
getJsonResponse :: Bool -> T.Text -> Value
getJsonResponse isEphemeral queryResponse = object [
    "response_type" .= T.pack (if isEphemeral then "ephemeral" else "in_channel"),
    "text" .= queryResponse]

