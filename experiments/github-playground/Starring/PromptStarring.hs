{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GitHub as GH
import qualified GitHub.Endpoints.Activity.Starring as GH

import Common (getAuth)
import qualified Common as GH
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified GHC.TypeError as T
import qualified Text.Read as T

parseRepoString :: T.Text -> Maybe (GH.Name GH.Owner, GH.Name GH.Repo)
parseRepoString input = case T.splitOn "/" input of
    [owner, repo] -> Just (GH.mkOwnerName owner, GH.mkRepoName repo)
    _ -> Nothing

starRepo :: (GH.Name GH.Owner, GH.Name GH.Repo) -> GH.Auth -> IO (Either GH.Error ())
starRepo (owner, repo) auth = GH.github auth $ GH.starRepoR owner repo

unstarRepo :: (GH.Name GH.Owner, GH.Name GH.Repo) -> GH.Auth -> IO (Either GH.Error ())
unstarRepo (owner, repo) auth = GH.github auth $ GH.unstarRepoR owner repo

promptRepo :: MaybeT IO (GH.Name GH.Owner, GH.Name GH.Repo)
promptRepo = do
    liftIO $ T.putStrLn "Enter a repo in the format \"owner/repo\""
    input <- liftIO T.getLine
    MaybeT $ pure $ parseRepoString input

starRepoAction :: MaybeT IO ()
starRepoAction = do
    promptRepo >>= \(owner, repo) -> do
        auth <- MaybeT getAuth

        result <- liftIO $ starRepo (owner, repo) auth
        case result of
            Left err -> liftIO $ putStrLn $ "Error: " <> show err
            Right () ->
                liftIO $
                    putStrLn $
                        "Starred: "
                            <> T.unpack (GH.untagName owner)
                            <> "/"
                            <> T.unpack (GH.untagName repo)

unstarRepoAction :: MaybeT IO ()
unstarRepoAction = do
    promptRepo >>= \(owner, repo) -> do
        auth <- MaybeT getAuth

        result <- liftIO $ unstarRepo (owner, repo) auth
        case result of
            Left err -> liftIO $ putStrLn $ "Error: " <> show err
            Right () ->
                liftIO $
                    putStrLn $
                        "Unstarred: "
                            <> T.unpack (GH.untagName owner)
                            <> "/"
                            <> T.unpack (GH.untagName repo)

readAction :: IO (Maybe Int)
readAction = T.readMaybe . T.unpack <$> T.getLine

main :: IO ()
main = void $ runMaybeT $ do
    liftIO $ T.putStrLn "Choose an action:"
    liftIO $ T.putStrLn "  1. Star a repo"
    liftIO $ T.putStrLn "  2. Unstar a repo"
    liftIO $ T.putStrLn "  3. Exit"

    action <- liftIO readAction
    case action of
        Just 1 -> starRepoAction
        Just 2 -> unstarRepoAction
        Just 3 -> liftIO $ T.putStrLn "Exiting"
        _ -> liftIO $ T.putStrLn "Invalid action"
