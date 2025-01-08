{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (getAuth)
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn)
import qualified Data.Vector as V
import qualified GitHub as GH

printRepo :: GH.Repo -> IO ()
printRepo repo =
    T.putStrLn $
        T.concat
            [ GH.untagName $ GH.simpleOwnerLogin $ GH.repoOwner repo
            , "/"
            , GH.untagName $ GH.repoName repo
            ]

main :: IO ()
main = do
    maybeAuth <- getAuth
    case maybeAuth of
        Nothing -> putStrLn "Error: No auth token"
        Just auth -> do
            result <- GH.github auth GH.myStarredR GH.FetchAll
            case result of
                Left err -> putStrLn $ "Error: " ++ show err
                Right repos -> mapM_ printRepo repos
