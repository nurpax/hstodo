{-# LANGUAGE OverloadedStrings #-}

module Model.Types (
    User(..)
  , Tag(..)
  , Todo(..)
  , TodoId(..)
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
  { tagId   :: Int64
  , tagText :: T.Text
  } deriving (Show, Eq)

newtype TodoId = TodoId { unTodoId :: Int64 } deriving (Show, Eq)

data Todo =
  Todo
  { todoId   :: Maybe TodoId
  , todoText :: T.Text
  , todoDone :: Bool
  , todoTags :: [Tag]
  } deriving (Show, Eq)

instance FromJSON Tag where
  parseJSON (Object v) =
    Tag <$> v .: "id"
        <*> v .: "tag"
  parseJSON _ = mzero

instance ToJSON Tag where
  toJSON (Tag i tag) =
    object [ "id"  .= i
           , "tag" .= tag
           ]

instance FromJSON Todo where
  parseJSON (Object v) =
    Todo <$> optional (TodoId <$> (v .: "id"))
         <*> v .: "text"
         <*> v .: "done"
         <*> (maybeToList <$> optional (v .: "tags"))
  parseJSON _ = mzero

instance ToJSON Todo where
  toJSON (Todo i text done tags) =
    object [ "id"   .= (unTodoId . fromJust $ i)
           , "text" .= text
           , "done" .= done
           , "tags" .= tags
           ]

