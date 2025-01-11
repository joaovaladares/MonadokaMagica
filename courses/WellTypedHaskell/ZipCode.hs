module ZipCode
  ( ZipCode
  , mkZipCode
  , getZipCode
  ) where

newtype ZipCode = ZipCode String
  deriving (Show, Eq, Ord)

mkZipCode :: String -> Maybe ZipCode
mkZipCode z
  | length z == 5 && all (`elem` ['0'..'9']) z = Just (ZipCode z) 
  | otherwise                                  = Nothing

getZipCode :: ZipCode -> String
getZipCode (ZipCode z) = z
