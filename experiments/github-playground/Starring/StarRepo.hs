{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GitHub as GH
import qualified GitHub.Endpoints.Activity.Starring as GH

import Common (getAuth)
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
    maybeAuth <- getAuth
    case maybeAuth of
        Nothing -> putStrLn "Error: No auth token"
        Just auth -> do
            let owner = "joaovaladares"
                repo = "MonadokaMagica"
            result <-
                GH.github auth $
                    GH.starRepoR (GH.mkOwnerName owner) (GH.mkRepoName repo)
            case result of
                Left err -> putStrLn $ "Error: " ++ show err
                Right () -> T.putStrLn $ T.concat ["Starred: ", owner, "/", repo]
