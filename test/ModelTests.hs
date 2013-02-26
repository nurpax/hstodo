{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import           Control.Exception hiding (Handler)
import           Control.Monad
import           Data.Int (Int64)
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Data.Time (UTCTime, getCurrentTime, secondsToDiffTime, addUTCTime)
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
withNewDb action =
  bracket (S.open ":memory:") S.close (\c -> M.createTables c >> action c)

mkDefaultTodo :: T.Text -> M.Todo
mkDefaultTodo text = M.Todo Nothing text False Nothing []

mkTodoId :: Integral n => n -> Maybe M.TodoId
mkTodoId = Just . M.TodoId . fromIntegral

mkNoteId :: Integral n => n -> Maybe M.NoteId
mkNoteId = Just . M.NoteId . fromIntegral

defaultListTodos :: S.Connection -> M.User -> IO [M.Todo]
defaultListTodos c u = M.listTodos c u Nothing

testSaveTodo :: Test
testSaveTodo = testCase "save todo" $
  withNewDb $ \c -> do
    todos <- defaultListTodos c defaultUser
    [] @=? todos
    let todo1 = mkDefaultTodo "test1"
    let todo2 = mkDefaultTodo "test2"
    todo1' <- M.saveTodo c defaultUser todo1
    todo1' @?= todo1 { M.todoId = mkTodoId 1 }
    todos <- defaultListTodos c defaultUser
    [todo1'] @=? todos
    todo2' <- M.saveTodo c defaultUser todo2
    todos <- defaultListTodos c defaultUser
    [todo1', todo2'] @=? todos

testUpdateTodo :: Test
testUpdateTodo = testCase "update todo" $
  withNewDb $ \c -> do
    todos <- defaultListTodos c defaultUser
    [] @=? todos
    let todo = mkDefaultTodo "test1"
    todo' <- M.saveTodo c defaultUser todo
    todo' @?= todo { M.todoId = mkTodoId 1 }
    updated <- M.saveTodo c defaultUser (todo' { M.todoText = "updated text" })
    "updated text" @=? M.todoText updated
    mkTodoId 1 @=? M.todoId updated
    todos <- defaultListTodos c defaultUser
    [updated] @=? todos

testUpdateActivatesTodo :: Test
testUpdateActivatesTodo = testCase "update todo activates" $
  withNewDb $ \c -> do
    let activatesTime  = read "2012-08-20 00:00:00" :: UTCTime
    let activatesTime2 = read "2012-05-20 00:10:00" :: UTCTime
    let todo = (mkDefaultTodo "test1") { M.todoActivatesOn = Just activatesTime }
    todo' <- M.saveTodo c defaultUser todo
    todo' @?= todo { M.todoId = mkTodoId 1 }
    Just activatesTime @?= M.todoActivatesOn todo'
    updated <- M.saveTodo c defaultUser
                 (todo' { M.todoText = "updated text"
                        , M.todoActivatesOn = Just activatesTime2 })
    "updated text" @=? M.todoText updated
    mkTodoId 1 @=? M.todoId updated
    Just activatesTime2 @?= M.todoActivatesOn updated
    todos <- defaultListTodos c defaultUser
    [updated] @=? todos

testActivatesList :: Test
testActivatesList = testCase "get list of todos based on activatesOn" $
  withNewDb $ \c -> do
    let activatesTime  = read "2012-08-20 00:00:00" :: UTCTime
    today <- getCurrentTime
    let todo = (mkDefaultTodo "test1") { M.todoActivatesOn = Just activatesTime }
    todo' <- M.saveTodo c defaultUser todo
    todos <- M.listTodos c defaultUser Nothing
    todos @=? [todo']
    todos <- M.listTodos c defaultUser (Just (M.TodoFilter True (Just today)))
    todos @=? [todo']

testActivatesList2 :: Test
testActivatesList2 = testCase "get list of todos based on activatesOn" $
  withNewDb $ \c -> do
    today <- getCurrentTime
    let todo = (mkDefaultTodo "test1") { M.todoActivatesOn = Just today }
    todo' <- M.saveTodo c defaultUser todo
    todos <- M.listTodos c defaultUser Nothing
    todos @?= [todo']
    -- Should still find the created todo
    todos <- M.listTodos c defaultUser (Just . M.TodoFilter True $ Just today)
    todos @?= [todo']
    -- Spoof list filter one day earlier - so that the todo we created
    -- above appears to trigger tomorrow -> shouldn't show up on the
    -- list.
    let yesterday = addUTCTime (-60*60*24) today
    todos <- M.listTodos c defaultUser (Just . M.TodoFilter True $ Just yesterday)
    todos @?= []

testActivatesList3 :: Test
testActivatesList3 = testCase "get list of todos based on activatesOn" $
  withNewDb $ \c -> do
    today <- getCurrentTime
    let todo = (mkDefaultTodo "test1") { M.todoActivatesOn = Nothing }
    todo' <- M.saveTodo c defaultUser todo
    todos <- M.listTodos c defaultUser Nothing
    [todo'] @=? todos
    -- Should still find the created todo
    todos <- M.listTodos c defaultUser (Just . M.TodoFilter True $ Just today)
    [todo'] @=? todos

testListUndone :: Test
testListUndone = testCase "get list undone todos" $
  withNewDb $ \c -> do
    let todo1 = (mkDefaultTodo "test1") { M.todoDone = True }
    let todo2 = (mkDefaultTodo "test2") { M.todoDone = False }
    todo1' <- M.saveTodo c defaultUser todo1
    todo2' <- M.saveTodo c defaultUser todo2
    todos <- M.listTodos c defaultUser Nothing
    [todo1', todo2'] @=? todos
    todos <- M.listTodos c defaultUser (Just $ M.TodoFilter False Nothing)
    [todo2'] @=? todos


testNewTag :: Test
testNewTag = testCase "new tag" $
  withNewDb $ \c -> do
    let todo = mkDefaultTodo "test1"
    todo' <- M.saveTodo c defaultUser todo
    let todoId_ = fromJust . M.todoId $ todo'
    [t] <- M.addTodoTag c defaultUser todoId_ "foo"
    "foo" @=? M.tagText t
    [_,t2] <- M.addTodoTag c defaultUser todoId_ "bar"
    ["foo", "bar"] @=? map M.tagText [t, t2]

testRemoveTag :: Test
testRemoveTag = testCase "remove tag" $
  withNewDb $ \c -> do
    let t = mkDefaultTodo "test1"
    todo <- M.saveTodo c defaultUser t
    let todoId_ = fromJust . M.todoId $ todo
    [tag] <- M.addTodoTag c defaultUser todoId_ "foo"
    M.Tag 1 "foo" @=? tag
    [tag, tag2] <- M.addTodoTag c defaultUser todoId_ "bar"
    [t'] <- defaultListTodos c defaultUser
    [tag, tag2] @=? M.todoTags t'
    newTags <- M.removeTodoTag c todoId_ tag
    [t'] <- defaultListTodos c defaultUser
    [tag2] @=? newTags
    [tag2] @=? M.todoTags t'
    t <- M.removeTodoTag c todoId_ tag2
    [t'] <- defaultListTodos c defaultUser
    [] @=? t
    [] @=? M.todoTags t'

testNewTagDupes :: Test
testNewTagDupes = testCase "duplicate tags" $
  withNewDb $ \c -> do
    let todo = mkDefaultTodo "test1"
    todo' <- M.saveTodo c defaultUser todo
    let todoId_ = fromJust . M.todoId $ todo'
    [t] <- M.addTodoTag c defaultUser todoId_ "foo"
    M.Tag 1 "foo" @=? t
    "foo" @=? M.tagText t
    -- Second addTag shouldn't lead to a todo having the same twice
    [t] <- M.addTodoTag c defaultUser todoId_ "foo"
    "foo" @=? M.tagText t

takeTodoId :: M.Todo -> M.TodoId
takeTodoId = fromJust . M.todoId

testNewTagDupesMultiUser :: Test
testNewTagDupesMultiUser = testCase "duplicate tags (multi-user)" $
  withNewDb $ \c -> do
    let t = mkDefaultTodo "test1"
    todoUsr1 <- M.saveTodo c defaultUser  t
    todoUsr2 <- M.saveTodo c defaultUser2 t
    t <- M.addTodoTag c defaultUser (takeTodoId todoUsr1) "foo"
    -- Second addTag shouldn't lead to a todo having the same twice
    t1 <- M.addTodoTag c defaultUser (takeTodoId todoUsr1) "foo"
    ["foo"] @=? map M.tagText t1
    t2 <- M.addTodoTag c defaultUser2 (takeTodoId todoUsr2) "foo"
    ["foo"] @=? map M.tagText t2

testSaveNote :: Test
testSaveNote = testCase "save note" $
  withNewDb $ \c -> do
    notes <- M.listNotes c defaultUser
    [] @=? notes
    let note1 = M.Note Nothing "test1" "test1 body" []
    let note2 = M.Note Nothing "test2" "test2 body" []
    note1' <- M.saveNote c defaultUser note1
    note1' @?= note1 { M.noteId = mkNoteId 1 }
    notes <- M.listNotes c defaultUser
    [note1'] @=? notes
    note2' <- M.saveNote c defaultUser note2
    notes <- M.listNotes c defaultUser
    [note1', note2'] @=? notes

testUpdateNote :: Test
testUpdateNote = testCase "update note" $
  withNewDb $ \c -> do
    notes <- M.listNotes c defaultUser
    [] @=? notes
    let note = M.Note Nothing "test1 title" "test1 body" []
    note' <- M.saveNote c defaultUser note
    note' @?= note { M.noteId = mkNoteId 1 }
    updated <- M.saveNote c defaultUser (note' { M.noteText = "updated text" })
    "updated text" @=? M.noteText updated
    "test1 title" @=? M.noteTitle updated
    mkNoteId 1 @=? M.noteId updated
    notes <- M.listNotes c defaultUser
    [updated] @=? notes

testTagNote :: Test
testTagNote = testCase "tag a note" $
  withNewDb $ \c -> do
    let note = M.Note Nothing "test1 title" "test1 body" []
    note' <- M.saveNote c defaultUser note
    let noteId_ = fromJust . M.noteId $ note'
    t1 <- M.addNoteTag c defaultUser noteId_ "foo"
    t2 <- M.addNoteTag c defaultUser noteId_ "bar"
    ["foo"] @=? map M.tagText t1
    ["foo", "bar"] @=? map M.tagText t2

testRemoveTagNote :: Test
testRemoveTagNote = testCase "remove tag" $
  withNewDb $ \c -> do
    let t = M.Note Nothing "test1 title" "test1 body" []
    note <- M.saveNote c defaultUser t
    let noteId_ = fromJust . M.noteId $ note
    [tag] <- M.addNoteTag c defaultUser noteId_ "foo"
    [_,tag2] <- M.addNoteTag c defaultUser noteId_ "bar"
    [n] <- M.listNotes c defaultUser
    ["foo", "bar"] @=? map M.tagText (M.noteTags n)
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
  defaultMain tests `finally` return ()

  where
    tests =
      [ mutuallyExclusive $ testGroup "todo tests"
        [ testSaveTodo
        , testUpdateTodo
        , testUpdateActivatesTodo
        , testActivatesList
        , testActivatesList2
        , testActivatesList3
        , testListUndone
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
