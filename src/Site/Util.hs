{-# LANGUAGE OverloadedStrings #-}

module Site.Util (
    reader
  , logFail
  , logRunEitherT
  , getIntParam
  , getTextParam
  ) where

import           Control.Error.Util (noteT, hoistMaybe)
import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import           Data.ByteString (ByteString)
import           Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth

import           Site.Application

type H = Handler App (AuthManager App)

reader :: T.Reader a -> T.Text -> Either String a
reader p s =
  case p s of
    Right (a, "") -> return a
    Right (_, _) -> Left "readParser: input not exhausted"
    Left e -> Left e

-- | Log Either Left values or run the Handler action.  To be used in
-- situations where to user shouldn't see an error (either due to it
-- being irrelevant or due to security) but we want to leave a trace
-- of the error case anyway.
logFail :: Either String (H ()) -> H ()
logFail = either (logError . T.encodeUtf8 . T.pack) id

logRunEitherT :: EitherT String H (H ()) -> H ()
logRunEitherT e = runEitherT e >>= logFail

getParamE :: (ByteString -> Either String a) -> ByteString -> EitherT String H a
getParamE f s = do
  v <- lift $ getParam s
  vm <- noteT "missing param" $ hoistMaybe v
  hoistEither (f vm)

getIntParam :: ByteString -> EitherT String H Int64
getIntParam = getParamE (reader T.decimal . T.decodeUtf8)

getTextParam :: ByteString -> EitherT String H T.Text
getTextParam = getParamE (Right . T.decodeUtf8)
