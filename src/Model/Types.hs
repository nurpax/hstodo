{-# LANGUAGE OverloadedStrings #-}

module Model.Types (
    User(..)
  , Tag(..)
  , Todo(..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Int (Int64)
import           Data.Maybe
import qualified Data.Text as T

data User = User Int T.Text

data Tag =
  Tag
  { tagId   :: Maybe Int64
  , tagText :: T.Text
  } deriving (Show, Eq)

data Todo =
  Todo
  { todoId   :: Maybe Int64
  , todoText :: T.Text
  , todoDone :: Bool
  , todoTags :: [Tag]
  } deriving (Show, Eq)

instance FromJSON Tag where
  parseJSON (Object v) =
    Tag <$> optional (v .: "id")
        <*> v .: "tag"
  parseJSON _ = mzero

instance ToJSON Tag where
  toJSON (Tag i tag) =
    object [ "id"  .= fromJust i
           , "tag" .= tag
           ]

instance FromJSON Todo where
  parseJSON (Object v) =
    Todo <$> optional (v .: "id")
         <*> v .: "text"
         <*> v .: "done"
         <*> (maybeToList <$> optional (v .: "tags"))
  parseJSON _ = mzero

instance ToJSON Todo where
  toJSON (Todo i text done tags) =
    object [ "id" .= fromJust i
           , "text" .= text
           , "done" .= done
           , "tags" .= tags
           ]

