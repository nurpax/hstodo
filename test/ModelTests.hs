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

mkNoteId :: Integral n => n -> Maybe M.NoteId
mkNoteId = Just . M.NoteId . fromIntegral

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
    t <- M.addTodoTag c todoId_ tag
    tag2 <- M.newTag c defaultUser "bar"
    [tag] @=? t
    t <- M.addTodoTag c todoId_ tag2
    [tag, tag2] @=? t

testRemoveTag :: Test
testRemoveTag = testCase "remove tag" $ do
  withNewDb $ \c -> do
    let t = (M.Todo Nothing "test1" False [])
    todo <- M.saveTodo c defaultUser t
    let todoId_ = fromJust . M.todoId $ todo
    tag <- M.newTag c defaultUser "foo"
    M.Tag 1 "foo" @=? tag
    t <- M.addTodoTag c todoId_ tag
    tag2 <- M.newTag c defaultUser "bar"
    [tag] @=? t
    newTags <- M.addTodoTag c todoId_ tag2
    [t'] <- M.listTodos c defaultUser
    [tag, tag2] @=? newTags
    [tag, tag2] @=? M.todoTags t'
    newTags <- M.removeTodoTag c todoId_ tag
    [t'] <- M.listTodos c defaultUser
    [tag2] @=? newTags
    [tag2] @=? M.todoTags t'
    t <- M.removeTodoTag c todoId_ tag2
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
    t <- M.addTodoTag c todoId_ tag
    [tag] @=? t
    -- Second addTag shouldn't lead to a todo having the same twice
    t <- M.addTodoTag c todoId_ tag
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
    t <- M.addTodoTag c todoId1 tagUsr1
    [tagUsr1] @=? t
    -- Second addTag shouldn't lead to a todo having the same twice
    t <- M.addTodoTag c todoId1 tagUsr1
    [tagUsr1] @=? t
    t <- M.addTodoTag c todoId2 tagUsr2
    [tagUsr2] @=? t

testSaveNote :: Test
testSaveNote = testCase "save note" $ do
  withNewDb $ \c -> do
    notes <- M.listNotes c defaultUser
    [] @=? notes
    let note1 = (M.Note Nothing "test1" "test1 body" [])
    let note2 = (M.Note Nothing "test2" "test2 body" [])
    note1' <- M.saveNote c defaultUser note1
    note1' @?= note1 { M.noteId = mkNoteId 1 }
    notes <- M.listNotes c defaultUser
    [note1'] @=? notes
    note2' <- M.saveNote c defaultUser note2
    notes <- M.listNotes c defaultUser
    [note1', note2'] @=? notes

testUpdateNote :: Test
testUpdateNote = testCase "update note" $ do
  withNewDb $ \c -> do
    notes <- M.listNotes c defaultUser
    [] @=? notes
    let note = (M.Note Nothing "test1 title" "test1 body" [])
    note' <- M.saveNote c defaultUser note
    note' @?= note { M.noteId = mkNoteId 1 }
    updated <- M.saveNote c defaultUser (note' { M.noteText = "updated text" })
    "updated text" @=? M.noteText updated
    "test1 title" @=? M.noteTitle updated
    mkNoteId 1 @=? M.noteId updated
    notes <- M.listNotes c defaultUser
    [updated] @=? notes

testTagNote :: Test
testTagNote = testCase "tag a note" $ do
  withNewDb $ \c -> do
    let note = (M.Note Nothing "test1 title" "test1 body" [])
    note' <- M.saveNote c defaultUser note
    let noteId_ = fromJust . M.noteId $ note'
    tag <- M.newTag c defaultUser "foo"
    M.Tag 1 "foo" @=? tag
    t <- M.addNoteTag c noteId_ tag
    tag2 <- M.newTag c defaultUser "bar"
    [tag] @=? t
    t <- M.addNoteTag c noteId_ tag2
    [tag, tag2] @=? t

testRemoveTagNote :: Test
testRemoveTagNote = testCase "remove tag" $ do
  withNewDb $ \c -> do
    let t = (M.Note Nothing "test1 title" "test1 body" [])
    note <- M.saveNote c defaultUser t
    let noteId_ = fromJust . M.noteId $ note
    tag <- M.newTag c defaultUser "foo"
    M.Tag 1 "foo" @=? tag
    t <- M.addNoteTag c noteId_ tag
    tag2 <- M.newTag c defaultUser "bar"
    [tag] @=? t
    newTags <- M.addNoteTag c noteId_ tag2
    [t'] <- M.listNotes c defaultUser
    [tag, tag2] @=? newTags
    [tag, tag2] @=? M.noteTags t'
    newTags <- M.removeNoteTag c noteId_ tag
    [t'] <- M.listNotes c defaultUser
    [tag2] @=? newTags
    [tag2] @=? M.noteTags t'
    t <- M.removeNoteTag c noteId_ tag2
    [t'] <- M.listNotes c defaultUser
    [] @=? t
    [] @=? M.noteTags t'

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
      , mutuallyExclusive $ testGroup "note tests"
        [ testSaveNote
        , testUpdateNote
        , testTagNote
        , testRemoveTagNote
        ]
      ]
