{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Model.Db (
    createTables
  , newTag
  , tagNameExists
  , listTags
    -- * Todos
  , queryTodo
  , saveTodo
  , listTodos
  , removeTodoTag
  , addTodoTag
    -- * Notes
  , saveNote
  , listNotes
  , addNoteTag
  , removeNoteTag
  , TodoId(..)
  , NoteId(..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Maybe (fromJust, listToMaybe)
import qualified Data.Text as T
import           Database.SQLite.Simple

import           Model.Types

instance FromRow Tag where
  fromRow = Tag <$> field <*> field

instance FromRow Todo where
  fromRow = Todo <$> fmap (Just . TodoId) field <*> field <*> field <*> pure []

instance FromRow Note where
  fromRow = Note <$> fmap (Just . NoteId) field <*> field <*> field <*> pure []

tableExists :: Connection -> String -> IO Bool
tableExists conn tblName = do
  r <- query conn "SELECT name FROM sqlite_master WHERE type='table' AND name=?" (Only tblName)
  case r of
    [Only (_ :: String)] -> return True
    _ -> return False

-- | Create the necessary database tables, if not already initialized.
createTables :: Connection -> IO ()
createTables conn = do
  -- Note: for a bigger app, you probably want to create a 'version'
  -- table too and use it to keep track of schema version and
  -- implement your schema upgrade procedure here.
  schemaCreated <- tableExists conn "todos"
  unless schemaCreated $ do
    execute_ conn "BEGIN"
    execute_ conn
      (Query $
       T.concat [ "CREATE TABLE todos ("
                , "id       INTEGER PRIMARY KEY, "
                , "user_id  INTEGER NOT NULL, "
                , "saved_on TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL, "
                , "text     TEXT, "
                , "done     BOOLEAN)"])
    execute_ conn
      (Query $
       T.concat [ "CREATE TABLE todo_tag_map ("
                , "todo_id INTEGER, "
                , "tag_id  INTEGER)"
                ])
    execute_ conn
      (Query $
       T.concat [ "CREATE TABLE notes ("
                , "id       INTEGER PRIMARY KEY, "
                , "user_id  INTEGER NOT NULL, "
                , "title    TEXT, "
                , "saved_on TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL, "
                , "text     TEXT)"])

    execute_ conn
      (Query $
       T.concat [ "CREATE TABLE note_tag_map ("
                , "note_id INTEGER, "
                , "tag_id  INTEGER)"
                ])
    execute_ conn
      (Query $
       T.concat [ "CREATE TABLE tags ("
                , "id      INTEGER PRIMARY KEY,"
                , "user_id INTEGER,"
                , "tag     TEXT,"
                , "UNIQUE(user_id, tag))"
                ])
    execute_ conn "COMMIT"

-- | User already has created a tag with the given name?
tagNameExists :: Connection -> User -> T.Text -> IO Bool
tagNameExists conn (User uid _) tag = do
  [Only nRows] <-
    query conn "SELECT count(*) FROM tags WHERE user_id = ? AND tag = ?"
      (uid, tag) :: IO [Only Int]
  return $ nRows /= 0

-- | Is a Tag already associated with a given Todo item
todoHasTag :: Connection -> TodoId -> Tag -> IO Bool
todoHasTag conn (TodoId todoId_) tag = do
  [Only nRows] <-
    query conn "SELECT count(*) FROM todo_tag_map WHERE todo_id = ? AND tag_id = ?"
      (todoId_, tagId tag) :: IO [Only Int]
  return $ nRows /= 0

-- | Look for a Tag by its name
-- Note: use only if you know the tag exists
tagByName :: Connection -> User -> T.Text -> IO Tag
tagByName conn (User uid _) tag = do
  [t] <- query conn "SELECT id,tag FROM tags WHERE user_id = ? and tag = ?" (uid, tag)
  return t

-- | Create a new tag
newTag :: Connection -> User -> T.Text -> IO Tag
newTag conn user@(User uid _) t = do
  tagExists <- tagNameExists conn user t
  if not tagExists then do
    execute conn "INSERT INTO tags (user_id, tag) VALUES (?, ?)" (uid, t)
    rowId <- lastInsertRowId conn
    return $ Tag rowId t
   else
    tagByName conn user t


listTodoTags :: Connection -> TodoId -> IO [Tag]
listTodoTags conn (TodoId todo) =
  query conn
    (Query $
       T.concat [ "SELECT tags.id,tags.tag FROM tags, todo_tag_map "
                , "WHERE todo_tag_map.todo_id = ? AND todo_tag_map.tag_id = tags.id"
                ])
    (Only todo)

-- | Retrieve a user's list of todos
listTodos' :: Connection -> User -> Maybe TodoId -> IO [Todo]
listTodos' conn (User uid _) todoId_  = do
  todos <-
    case todoId_ of
      Nothing ->
        query conn "SELECT id,text,done FROM todos WHERE user_id = ?" (Only uid)
      Just (TodoId tid) ->
        query conn "SELECT id,text,done FROM todos WHERE (user_id = ? AND id = ?)" (uid, tid)
  mapM queryTags todos
  where
    queryTags todo = do
      tags <- listTodoTags conn (fromJust . todoId $ todo)
      return $ todo { todoTags = tags }

listTodos :: Connection -> User -> IO [Todo]
listTodos c u = listTodos' c u Nothing

queryTodo :: Connection -> User -> TodoId -> IO (Maybe Todo)
queryTodo conn user todoId_ = do
  todos <- listTodos' conn user (Just todoId_)
  return . listToMaybe $ todos


-- | Save or update a todo
saveTodo :: Connection -> User -> Todo -> IO Todo
saveTodo conn user@(User uid _) t =
  maybe newTodo updateTodo (todoId t)
  where
    newTodo = do
      execute conn "INSERT INTO todos (user_id,text,done) VALUES (?,?,?)"
        (uid, todoText t, todoDone t)
      rowId <- lastInsertRowId conn
      return $ t { todoId = Just . TodoId $ rowId }

    updateTodo tid = do
      execute conn "UPDATE todos SET text = ?, done = ? WHERE (user_id = ? AND id = ?)"
        (todoText t, todoDone t, uid, unTodoId tid)
      fromJust <$> queryTodo conn user tid

-- | Assign a Tag to a given Todo
addTodoTag :: Connection -> TodoId -> Tag -> IO [Tag]
addTodoTag conn todo@(TodoId todoId_) tag = do
  tagAlreadySet <- todoHasTag conn todo tag
  unless tagAlreadySet $
    execute conn "INSERT INTO todo_tag_map (todo_id, tag_id) VALUES (?,?)"
      (todoId_, tagId tag)
  listTodoTags conn todo

-- | Remove a Tag from a given Todo
removeTodoTag :: Connection -> TodoId -> Tag -> IO [Tag]
removeTodoTag conn todo@(TodoId todoId_) tag = do
  execute conn "DELETE FROM todo_tag_map WHERE todo_id = ? AND tag_id = ?"
    (todoId_, tagId tag)
  listTodoTags conn todo


listNoteTags :: Connection -> NoteId -> IO [Tag]
listNoteTags conn (NoteId id_) =
  query conn
    (Query $
       T.concat [ "SELECT tags.id,tags.tag FROM tags, note_tag_map "
                , "WHERE note_tag_map.note_id = ? AND note_tag_map.tag_id = tags.id"
                ])
    (Only id_)

-- | Retrieve a user's list of notes
listNotes' :: Connection -> User -> Maybe NoteId -> IO [Note]
listNotes' conn (User uid _) id_  = do
  notes <-
    case id_ of
      Nothing ->
        query conn "SELECT id,title,text FROM notes WHERE user_id = ?" (Only uid)
      Just (NoteId nid) ->
        query conn "SELECT id,title,text FROM notes WHERE (user_id = ? AND id = ?)" (uid, nid)
  mapM queryTags notes
  where
    queryTags note = do
      tags <- listNoteTags conn (fromJust . noteId $ note)
      return $ note { noteTags = tags }

listNotes :: Connection -> User -> IO [Note]
listNotes c u = listNotes' c u Nothing

queryNote :: Connection -> User -> NoteId -> IO (Maybe Note)
queryNote conn user noteId_ = do
  listNotes' conn user (Just noteId_) >>= return . listToMaybe

-- | Save or update a note
saveNote :: Connection -> User -> Note -> IO Note
saveNote conn user@(User uid _) n =
  maybe new update (noteId n)
  where
    new = do
      execute conn "INSERT INTO notes (user_id,title,text) VALUES (?,?,?)"
        (uid, noteTitle n, noteText n)
      rowId <- lastInsertRowId conn
      return $ n { noteId = Just . NoteId $ rowId }

    update nid = do
      execute conn "UPDATE notes SET title = ?, text = ? WHERE (user_id = ? AND id = ?)"
        (noteTitle n, noteText n, uid, unNoteId nid)
      fromJust <$> queryNote conn user nid


-- | Is a Tag already associated with a given Note item
noteHasTag :: Connection -> NoteId -> Tag -> IO Bool
noteHasTag conn (NoteId noteId_) tag = do
  [Only nRows] <-
    query conn "SELECT count(*) FROM note_tag_map WHERE note_id = ? AND tag_id = ?"
      (noteId_, tagId tag) :: IO [Only Int]
  return $ nRows /= 0

-- | Assign a Tag to a given Note
addNoteTag :: Connection -> NoteId -> Tag -> IO [Tag]
addNoteTag conn note@(NoteId noteId_) tag = do
  tagAlreadySet <- noteHasTag conn note tag
  unless tagAlreadySet $
    execute conn "INSERT INTO note_tag_map (note_id, tag_id) VALUES (?,?)"
      (noteId_, tagId tag)
  listNoteTags conn note

-- | Remove a Tag from a given Note
removeNoteTag :: Connection -> NoteId -> Tag -> IO [Tag]
removeNoteTag conn note@(NoteId noteId_) tag = do
  execute conn "DELETE FROM note_tag_map WHERE note_id = ? AND tag_id = ?"
    (noteId_, tagId tag)
  listNoteTags conn note

-- | Retrieve a user's list of tags
listTags :: Connection -> User -> IO [Tag]
listTags conn (User uid _) =
  query conn "SELECT id,tag FROM tags WHERE user_id = ?" (Only uid)
