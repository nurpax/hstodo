{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import           Control.Exception hiding (Handler)
import           Control.Monad
import           Prelude hiding (catch)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)

import qualified Database.SQLite.Simple as S

import qualified Model.Db as Db

defaultUser :: Db.User
defaultUser = Db.User 1 "test"

withNewDb :: (S.Connection -> IO a) -> IO a
withNewDb action = do
  bracket (S.open ":memory:") S.close (\c -> Db.createTables c >> action c)

testSaveTodo :: Test
testSaveTodo = testCase "save todo" $ do
  withNewDb $ \c -> do
    todos <- Db.listTodos c defaultUser
    [] @=? todos
    let todo1 = (Db.Todo Nothing "test1" False [])
    let todo2 = (Db.Todo Nothing "test2" False [])
    todo1' <- Db.saveTodo c defaultUser todo1
    todo1' @?= todo1 { Db.todoId = Just 1 }
    todos <- Db.listTodos c defaultUser
    [todo1'] @=? todos
    todo2' <- Db.saveTodo c defaultUser todo2
    todos <- Db.listTodos c defaultUser
    [todo1', todo2'] @=? todos

testUpdateTodo :: Test
testUpdateTodo = testCase "update todo" $ do
  withNewDb $ \c -> do
    todos <- Db.listTodos c defaultUser
    [] @=? todos
    let todo = (Db.Todo Nothing "test1" False [])
    todo' <- Db.saveTodo c defaultUser todo
    todo' @?= todo { Db.todoId = Just 1 }
    updated <- Db.saveTodo c defaultUser (todo' { Db.todoText = "updated text" })
    "updated text" @=? Db.todoText updated
    Just 1 @=? Db.todoId updated
    todos <- Db.listTodos c defaultUser
    [updated] @=? todos

testNewTag :: Test
testNewTag = testCase "new tag" $ do
  withNewDb $ \c -> do
    let todo = (Db.Todo Nothing "test1" False [])
    todo' <- Db.saveTodo c defaultUser todo
    tag <- Db.newTag c "foo"
    Db.Tag 1 "foo" @=? tag
    t <- Db.addTag c todo' tag
    tag2 <- Db.newTag c "bar"
    [tag] @=? Db.todoTags t
    t <- Db.addTag c todo' tag2
    [tag, tag2] @=? Db.todoTags t

testRemoveTag :: Test
testRemoveTag = testCase "remove tag" $ do
  withNewDb $ \c -> do
    let t = (Db.Todo Nothing "test1" False [])
    todo <- Db.saveTodo c defaultUser t
    tag <- Db.newTag c "foo"
    Db.Tag 1 "foo" @=? tag
    t <- Db.addTag c todo tag
    tag2 <- Db.newTag c "bar"
    [tag] @=? Db.todoTags t
    t <- Db.addTag c todo tag2
    [t'] <- Db.listTodos c defaultUser
    [tag, tag2] @=? Db.todoTags t
    [tag, tag2] @=? Db.todoTags t'
    t <- Db.removeTag c todo tag
    [t'] <- Db.listTodos c defaultUser
    [tag2] @=? Db.todoTags t
    [tag2] @=? Db.todoTags t'
    t <- Db.removeTag c todo tag2
    [t'] <- Db.listTodos c defaultUser
    [] @=? Db.todoTags t
    [] @=? Db.todoTags t'

testNewTagDupes :: Test
testNewTagDupes = testCase "duplicate tags" $ do
  withNewDb $ \c -> do
    let todo = (Db.Todo Nothing "test1" False [])
    todo' <- Db.saveTodo c defaultUser todo
    tag <- Db.newTag c "foo"
    Db.Tag 1 "foo" @=? tag
    t <- Db.addTag c todo' tag
    [tag] @=? Db.todoTags t
    -- Second addTag shouldn't lead to a todo having the same twice
    t <- Db.addTag c todo' tag
    [tag] @=? Db.todoTags t

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
        , testRemoveTag
        ]
      ]
