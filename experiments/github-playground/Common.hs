{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Common (
    -- * Common stuff
    getAuth,
    tshow,

    -- * Re-exports
    putStrLn,
    getArgs,
    Proxy (..),
    module GitHub.Internal.Prelude,
) where

import GitHub.Internal.Prelude hiding (putStrLn)

import Configuration.Dotenv (defaultConfig, loadFile)
import Data.Proxy (Proxy (..))
import Data.Text.IO (putStrLn)
import System.Environment (getArgs, lookupEnv)

import qualified Data.Text as T
import qualified GitHub as GH

getAuth :: IO (Maybe GH.Auth)
getAuth = do
    loadFile defaultConfig
    token <- lookupEnv "GITHUB_TOKEN"
    case token of
        Nothing -> putStrLn "No token found" >> pure Nothing
        Just t -> pure $ Just $ GH.OAuth $ fromString t

tshow :: (Show a) => a -> Text
tshow = T.pack . show
