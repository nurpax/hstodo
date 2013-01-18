{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import           Control.Exception hiding (Handler)
import           Control.Monad
import           Data.Int (Int64)
import           Data.Maybe (fromJust)
import           Prelude hiding (catch)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)

import qualified Database.SQLite.Simple as S

import qualified Model as M

defaultUser :: M.User
defaultUser = M.User 1 "testUser"

defaultUser2 :: M.User
defaultUser2 = M.User 2 "testUser2"

withNewDb :: (S.Connection -> IO a) -> IO a
withNewDb action = do
  bracket (S.open ":memory:") S.close (\c -> M.createTables c >> action c)

mkTodoId :: Integral n => n -> Maybe M.TodoId
mkTodoId = Just . M.TodoId . fromIntegral

testSaveTodo :: Test
testSaveTodo = testCase "save todo" $ do
  withNewDb $ \c -> do
    todos <- M.listTodos c defaultUser
    [] @=? todos
    let todo1 = (M.Todo Nothing "test1" False [])
    let todo2 = (M.Todo Nothing "test2" False [])
    todo1' <- M.saveTodo c defaultUser todo1
    todo1' @?= todo1 { M.todoId = mkTodoId 1 }
    todos <- M.listTodos c defaultUser
    [todo1'] @=? todos
    todo2' <- M.saveTodo c defaultUser todo2
    todos <- M.listTodos c defaultUser
    [todo1', todo2'] @=? todos

testUpdateTodo :: Test
testUpdateTodo = testCase "update todo" $ do
  withNewDb $ \c -> do
    todos <- M.listTodos c defaultUser
    [] @=? todos
    let todo = (M.Todo Nothing "test1" False [])
    todo' <- M.saveTodo c defaultUser todo
    todo' @?= todo { M.todoId = mkTodoId 1 }
    updated <- M.saveTodo c defaultUser (todo' { M.todoText = "updated text" })
    "updated text" @=? M.todoText updated
    mkTodoId 1 @=? M.todoId updated
    todos <- M.listTodos c defaultUser
    [updated] @=? todos

testNewTag :: Test
testNewTag = testCase "new tag" $ do
  withNewDb $ \c -> do
    let todo = (M.Todo Nothing "test1" False [])
    todo' <- M.saveTodo c defaultUser todo
    let todoId_ = fromJust . M.todoId $ todo'
    tag <- M.newTag c defaultUser "foo"
    M.Tag 1 "foo" @=? tag
    t <- M.addTag c todoId_ tag
    tag2 <- M.newTag c defaultUser "bar"
    [tag] @=? t
    t <- M.addTag c todoId_ tag2
    [tag, tag2] @=? t

testRemoveTag :: Test
testRemoveTag = testCase "remove tag" $ do
  withNewDb $ \c -> do
    let t = (M.Todo Nothing "test1" False [])
    todo <- M.saveTodo c defaultUser t
    let todoId_ = fromJust . M.todoId $ todo
    tag <- M.newTag c defaultUser "foo"
    M.Tag 1 "foo" @=? tag
    t <- M.addTag c todoId_ tag
    tag2 <- M.newTag c defaultUser "bar"
    [tag] @=? t
    newTags <- M.addTag c todoId_ tag2
    [t'] <- M.listTodos c defaultUser
    [tag, tag2] @=? newTags
    [tag, tag2] @=? M.todoTags t'
    newTags <- M.removeTag c todoId_ tag
    [t'] <- M.listTodos c defaultUser
    [tag2] @=? newTags
    [tag2] @=? M.todoTags t'
    t <- M.removeTag c todoId_ tag2
    [t'] <- M.listTodos c defaultUser
    [] @=? t
    [] @=? M.todoTags t'

testNewTagDupes :: Test
testNewTagDupes = testCase "duplicate tags" $ do
  withNewDb $ \c -> do
    let todo = (M.Todo Nothing "test1" False [])
    todo' <- M.saveTodo c defaultUser todo
    let todoId_ = fromJust . M.todoId $ todo'
    tag <- M.newTag c defaultUser "foo"
    M.Tag 1 "foo" @=? tag
    t <- M.addTag c todoId_ tag
    [tag] @=? t
    -- Second addTag shouldn't lead to a todo having the same twice
    t <- M.addTag c todoId_ tag
    [tag] @=? t

testNewTagDupesMultiUser :: Test
testNewTagDupesMultiUser = testCase "duplicate tags (multi-user)" $ do
  withNewDb $ \c -> do
    let t = (M.Todo Nothing "test1" False [])
    todoUsr1 <- M.saveTodo c defaultUser  t
    todoUsr2 <- M.saveTodo c defaultUser2 t
    let todoId1 = fromJust . M.todoId $ todoUsr1
        todoId2 = fromJust . M.todoId $ todoUsr2
    tagUsr1 <- M.newTag c defaultUser "foo"
    tagUsr2 <- M.newTag c defaultUser2 "foo"
    M.Tag 1 "foo" @=? tagUsr1
    M.Tag 2 "foo" @=? tagUsr2
    t <- M.addTag c todoId1 tagUsr1
    [tagUsr1] @=? t
    -- Second addTag shouldn't lead to a todo having the same twice
    t <- M.addTag c todoId1 tagUsr1
    [tagUsr1] @=? t
    t <- M.addTag c todoId2 tagUsr2
    [tagUsr2] @=? t

main :: IO ()
main =
  defaultMain tests `finally` (return ())

  where
    tests =
      [ mutuallyExclusive $ testGroup "todo tests"
        [ testSaveTodo
        , testUpdateTodo
        ]
      , mutuallyExclusive $ testGroup "tag tests"
        [ testNewTag
        , testNewTagDupes
        , testNewTagDupesMultiUser
        , testRemoveTag
        ]
      ]
