{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (getAuth)
import qualified Common as GH
import qualified Data.Text as T
import qualified GitHub as GH

main :: IO ()
main = do
    let owner = GH.mkOwnerName "haskell-github"
        repo = GH.mkRepoName "github"
        issueNumber = GH.IssueNumber 517
    possibleIssue <-
        GH.github' $ GH.issueR owner repo issueNumber
    case possibleIssue of
        Left err -> putStrLn $ "Error: " ++ show err
        Right issue -> putStrLn $ formatIssue issue

formatIssue :: GH.Issue -> String
formatIssue issue =
    show (GH.simpleUserLogin $ GH.issueUser issue)
        ++ " opened issue #"
        ++ show (GH.issueNumber issue)
        ++ ": "
        ++ GH.unpack (GH.issueTitle issue)
        ++ " with "
        ++ show (GH.issueBody issue)
        ++ " comments"
