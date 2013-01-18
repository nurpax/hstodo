{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site.Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent (withMVar)
import           Control.Monad (mzero)
import           Control.Monad.Trans (liftIO, lift)
import           Control.Monad.Trans.Either
import           Control.Error.Safe (tryJust)
import           Control.Lens ((^#))
import           Data.Aeson
import           Data.ByteString (ByteString)
import           Data.Int (Int64)
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Read as T
import           Database.SQLite.Simple as S
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.SqliteSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.SqliteSimple
import           Snap.Util.FileServe
import           Snap.Extras.JSON
import           Heist()
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import qualified Model as M

import           Site.Application
import           Site.Util

type H = Handler App (AuthManager App)

-- | Render login form
handleLogin :: Maybe T.Text -> H ()
handleLogin authError =
  heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = [("loginError", I.textSplice c) | c <- maybeToList authError]

-- | Handle login submit.  Either redirect to '/' on success or give
-- an error.  We deliberately do NOT show the AuthFailure on the login
-- error, as we don't want to reveal to visitors whether or not the
-- login exists in the user database.
handleLoginSubmit :: H ()
handleLoginSubmit =
  loginUser "login" "password" Nothing
    (const . handleLogin . Just $ "Unknown login or incorrect password")
    (redirect "/")

-- | Logs out and redirects the user to the site index.
handleLogout :: H ()
handleLogout = logout >> redirect "/"

-- | Handle new user form submit
handleNewUser :: H ()
handleNewUser =
  method GET (renderNewUserForm Nothing) <|> method POST handleFormSubmit
  where
    handleFormSubmit = do
      authUser <- registerUser "login" "password"
      either (renderNewUserForm . Just) login authUser

    renderNewUserForm (err :: Maybe AuthFailure) =
      heistLocal (I.bindSplices errs) $ render "new_user"
      where
        errs = [("newUserError", I.textSplice . T.pack . show $ c) | c <- maybeToList err]

    login user = logRunEitherT $
      lift $ forceLogin user >> redirect "/"

-- | Run actions with a logged in user or go back to the login screen
withLoggedInUser :: (M.User -> H ()) -> H ()
withLoggedInUser action =
  currentUser >>= go
  where
    go Nothing  = handleLogin (Just "Must be logged in to view the main page")
    go (Just u) = logRunEitherT $ do
      uid  <- tryJust "withLoggedInUser: missing uid" (userId u)
      uid' <- hoistEither (reader T.decimal (unUid uid))
      return $ action (M.User uid' (userLogin u))

-- | Run an IO action with an SQLite connection
withDb :: (S.Connection -> IO a) -> H a
withDb action =
  withTop db . withSqlite $ \conn -> action conn

handleTodos :: H ()
handleTodos =
  method GET  (withLoggedInUser getTodos) <|>
  method POST (withLoggedInUser saveTodo)
  where
    getTodos user = do
      todos <- withDb $ \conn -> M.listTodos conn user
      writeJSON todos

    saveTodo user = do
      newTodo <- getJSON
      either (const $ return ()) persist newTodo
        where
          persist todo = do
            savedTodo <- withDb $ \conn -> M.saveTodo conn user todo
            writeJSON savedTodo

-- TODO lot of duplicate code here, find a way to reuse
handleTags :: H ()
handleTags =
  method GET  (withLoggedInUser getTags) <|>
  method POST (withLoggedInUser saveTag)
  where
    getTags user = do
      tags <- withDb $ \conn -> M.listTags conn user
      writeJSON tags

    saveTag user = do
      newTag <- getJSON
      either (liftIO . print) persist newTag
        where
          persist tag =
            -- TODO this will never update the tag?? is that ok?
            withDb (\conn -> M.newTag conn user tag) >>= writeJSON


data AddTagParams =
  AddTagParams
  { atpTodoId :: Int64
  , atpTag    :: T.Text
  }

instance FromJSON AddTagParams where
  parseJSON (Object v) =
    AddTagParams <$> v .: "todoId"
                 <*> v .: "tag"
  parseJSON _ = mzero

handleTodosAddTag :: H ()
handleTodosAddTag =
  method POST (withLoggedInUser todoAddTag)
  where
    todoAddTag user = do
      req <- getJSON
      either (liftIO . print) addTag req
      where
        addTag :: AddTagParams -> H ()
        addTag AddTagParams{..} = do
          Just tag <- withDb $ \c -> do
            newTag <- M.newTag c user atpTag
            M.addTag c (M.TodoId atpTodoId) newTag
            M.queryTodo c user (M.TodoId atpTodoId)
          writeJSON tag

-- | Render main page
mainPage :: H ()
mainPage = withLoggedInUser (const $ serveDirectory "static")

-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",        with auth handleLoginSubmit)
         , ("/logout",       with auth handleLogout)
         , ("/new_user",     with auth handleNewUser)
         , ("/api/todo/tag", with auth handleTodosAddTag)
         , ("/api/todo",     with auth handleTodos)
         , ("/api/tag",      with auth handleTags) -- TODO sort of not needed
         , ("/",             with auth mainPage)
         , ("/static",       serveDirectory "static")
         ]

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    -- addRoutes must be called before heistInit - heist wants to
    -- serve "" itself which means our mainPage handler never gets a
    -- chance to get called.
    addRoutes routes
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- Initialize auth that's backed by an sqlite database
    d <- nestSnaplet "db" db sqliteInit
    a <- nestSnaplet "auth" auth $ initSqliteAuth sess d

    -- Grab the DB connection pool from the sqlite snaplet and call
    -- into the Model to create all the DB tables if necessary.
    let conn = sqliteConn $ d ^# snapletValue
    liftIO $ withMVar conn M.createTables

    addAuthSplices auth
    return $ App h s d a

