{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (getAuth)
import qualified Common as GH
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
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
    let username = GH.unpack $ GH.untagName $ GH.simpleUserLogin $ GH.issueUser issue
        -- Remove the "N " prefix if it exists
        cleanUsername =
            if "N " `isPrefixOf` username
                then drop 2 username
                else username
     in cleanUsername
            ++ " opened issue #"
            ++ show (GH.issueNumber issue)
            ++ ": "
            ++ GH.unpack (GH.issueTitle issue)
            ++ "\n\n" -- Add spacing between title and body
            ++ formatIssueBody (GH.issueBody issue)
            ++ "\n" -- Add final newline
            ++ show (GH.issueComments issue)
            ++ " comments"

formatIssueBody :: Maybe T.Text -> String
formatIssueBody Nothing = ""
formatIssueBody (Just body) =
    let bodyStr = GH.unpack body
        -- Replace Windows-style line endings with Unix-style
        unixNewlines = replace "\\r\\n" "\n" bodyStr
        -- Split the text into segments based on code block markers
        segments = parseCodeBlocks unixNewlines
     in formatSegments segments

-- | Parse text into segments of regular text and code blocks
parseCodeBlocks :: String -> [Either String (String, String)] -- (language, code)
parseCodeBlocks text =
    let parts = splitOn "```" text
     in case parts of
            [] -> []
            (first : rest) -> parseSegments (Left first : zipWith makeSegment [1 ..] rest)
  where
    makeSegment idx content
        | odd idx =
            let ls = lines content
             in case ls of
                    [] -> Right ("", "")
                    (lang : code) -> Right (lang, unlines code)
        | otherwise = Left content

    parseSegments [] = []
    parseSegments (Left text : xs) = Left text : parseSegments xs
    parseSegments (Right (lang, code) : xs) = Right (lang, code) : parseSegments xs

-- | Format all segments into a single string
formatSegments :: [Either String (String, String)] -> String
formatSegments [] = ""
formatSegments (Left text : rest) = text ++ formatSegments rest
formatSegments (Right (lang, code) : rest) =
    "\nCode block ("
        ++ lang
        ++ "):\n"
        ++ unlines (map ("    " ++) (lines code))
        ++ formatSegments rest

-- | Simple string replacement helper
replace :: String -> String -> String -> String
replace old new = unlines . map (replaceLine old new) . lines
  where
    replaceLine o n s
        | o `isPrefixOf` s = n ++ drop (length o) s
        | otherwise = s
