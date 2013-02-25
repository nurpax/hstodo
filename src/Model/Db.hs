{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Model.Db (
    createTables
  , tagNameExists
  , listTags
    -- * Todos
  , queryTodo
  , saveTodo
  , listTodos
  , removeTodoTag
  , addTodoTag
    -- * Notes
  , queryNote
  , saveNote
  , listNotes
  , addNoteTag
  , removeNoteTag
  , TodoId(..)
  , NoteId(..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Int (Int64)
import           Data.Maybe (fromJust, listToMaybe)
import qualified Data.Text as T
import           Database.SQLite.Simple

import           Model.Types

data TagType = TagTodo | TagNote

instance FromRow Tag where
  fromRow = Tag <$> field <*> field

instance FromRow Todo where
  fromRow = Todo <$> fmap (Just . TodoId) field <*> field <*> field <*> field <*> pure []

instance FromRow Note where
  fromRow = Note <$> fmap (Just . NoteId) field <*> field <*> field <*> pure []

tableExists :: Connection -> String -> IO Bool
tableExists conn tblName = do
  r <- query conn "SELECT name FROM sqlite_master WHERE type='table' AND name=?" (Only tblName)
  case r of
    [Only (_ :: String)] -> return True
    _ -> return False

tagMapTableName :: TagType -> T.Text
tagMapTableName TagTodo = "todo_tag_map"
tagMapTableName TagNote = "note_tag_map"

createTagMapTable :: Connection -> TagType -> IO ()
createTagMapTable conn ty =
  execute_ conn
  (Query $
   T.concat [ "CREATE TABLE ", tagMapTableName ty, " ("
            , "object_id INTEGER, "
            , "tag_id    INTEGER)"
            ])

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
                , "id           INTEGER PRIMARY KEY, "
                , "user_id      INTEGER NOT NULL, "
                , "saved_on     TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL, "
                , "activates_on TIMESTAMP DEFAULT CURRENT_TIMESTAMP, "
                , "text         TEXT, "
                , "done         BOOLEAN)"])
    createTagMapTable conn TagTodo
    createTagMapTable conn TagNote
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
isObjectTagged :: Connection -> TagType -> Int64 -> Tag -> IO Bool
isObjectTagged conn tagType oid tag = do
  let q = Query $ T.concat [ "SELECT count(*) FROM "
                           , tagMapTableName tagType
                           , " WHERE object_id = ? AND tag_id = ?"
                           ]
  [Only nRows] <-
    query conn q (oid, tagId tag) :: IO [Only Int]
  return $ nRows /= 0

listObjectTags :: Connection -> TagType -> Int64 -> IO [Tag]
listObjectTags conn tagType oid =
  query conn
    (Query $
       T.concat [ "SELECT tags.id,tags.tag FROM tags, ", tagMapTableName tagType, " AS tmap "
                , " WHERE tmap.object_id = ? AND tmap.tag_id = tags.id"
                ])
    (Only oid)

-- | Assign a Tag to a given Object
tagObject :: Connection -> TagType -> Int64 -> Tag -> IO [Tag]
tagObject conn tagType oid tag = do
  tagAlreadySet <- isObjectTagged conn tagType oid tag
  unless tagAlreadySet $
    execute conn
      (Query $ T.concat ["INSERT INTO ", tagMapTableName tagType, " (object_id, tag_id) VALUES (?,?)"])
      (oid, tagId tag)
  listObjectTags conn tagType oid

-- | Remove a Tag from a given Object
untagObject :: Connection -> TagType -> Int64 -> Tag -> IO [Tag]
untagObject conn tagType oid tag = do
  execute conn
    (Query $ T.concat ["DELETE FROM ", tagMapTableName tagType, " WHERE object_id = ? AND tag_id = ?"])
    (oid, tagId tag)
  listObjectTags conn tagType oid


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
listTodoTags c (TodoId todo) = listObjectTags c TagTodo todo

-- | Retrieve a user's list of todos
listTodos' :: Connection -> User -> Maybe TodoFilter -> Maybe TodoId -> IO [Todo]
listTodos' conn (User uid _) listFilter todoId_  = do
  todos <-
    case todoId_ of
      Nothing ->
        case listFilter of
          Just (TodoFilter (Just activationDate)) ->
            let q = Query $ T.concat [ "SELECT id,text,done,activates_on FROM todos "
                                     , "WHERE user_id = ? "
                                     , "AND ((activates_on IS NULL) OR (activates_on <= ?))"
                                     ]
            in query conn q (uid, activationDate)
          _ ->
            query conn "SELECT id,text,done,activates_on FROM todos WHERE user_id = ?" (Only uid)
      Just (TodoId tid) ->
        query conn "SELECT id,text,done,activates_on FROM todos WHERE (user_id = ? AND id = ?)" (uid, tid)
  mapM queryTags todos
  where
    queryTags todo = do
      tags <- listTodoTags conn (fromJust . todoId $ todo)
      return $ todo { todoTags = tags }

listTodos :: Connection -> User -> Maybe TodoFilter -> IO [Todo]
listTodos c u f = listTodos' c u f Nothing

queryTodo :: Connection -> User -> TodoId -> IO (Maybe Todo)
queryTodo conn user todoId_ =
  listToMaybe <$> listTodos' conn user Nothing (Just todoId_)

-- | Save or update a todo
saveTodo :: Connection -> User -> Todo -> IO Todo
saveTodo conn user@(User uid _) t =
  maybe newTodo updateTodo (todoId t)
  where
    newTodo = do
      execute conn "INSERT INTO todos (user_id,text,done,activates_on) VALUES (?,?,?,?)"
        (uid, todoText t, todoDone t, todoActivatesOn t)
      rowId <- lastInsertRowId conn
      return $ t { todoId = Just . TodoId $ rowId }

    updateTodo tid = do
      let q = Query $ T.concat [ "UPDATE todos SET "
                               , "text = ?, done = ?, activates_on = ? "
                               , "WHERE (user_id = ? AND id = ?)"
                               ]
      execute conn q (todoText t, todoDone t, todoActivatesOn t, uid, unTodoId tid)
      fromJust <$> queryTodo conn user tid

addTodoTag :: Connection -> User -> TodoId -> T.Text -> IO [Tag]
addTodoTag c user (TodoId todo) text =
  newTag c user text >>= tagObject c TagTodo todo

removeTodoTag :: Connection -> TodoId -> Tag -> IO [Tag]
removeTodoTag c (TodoId todo) = untagObject c TagTodo todo

addNoteTag :: Connection -> User -> NoteId -> T.Text -> IO [Tag]
addNoteTag c user (NoteId note) text =
  newTag c user text >>= tagObject c TagNote note

removeNoteTag :: Connection -> NoteId -> Tag -> IO [Tag]
removeNoteTag c (NoteId note) = untagObject c TagNote note

listNoteTags :: Connection -> NoteId -> IO [Tag]
listNoteTags c (NoteId note) = listObjectTags c TagNote note

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
queryNote conn user noteId_ =
  listToMaybe <$> listNotes' conn user (Just noteId_)

-- | Save or update a note
saveNote :: Connection -> User -> Note -> IO Note
saveNote conn user@(User uid _) n =
  maybe insert update (noteId n)
  where
    insert = do
      execute conn "INSERT INTO notes (user_id,title,text) VALUES (?,?,?)"
        (uid, noteTitle n, noteText n)
      rowId <- lastInsertRowId conn
      return $ n { noteId = Just . NoteId $ rowId }

    update nid = do
      execute conn "UPDATE notes SET title = ?, text = ? WHERE (user_id = ? AND id = ?)"
        (noteTitle n, noteText n, uid, unNoteId nid)
      fromJust <$> queryNote conn user nid


-- | Retrieve a user's list of tags
listTags :: Connection -> User -> IO [Tag]
listTags conn (User uid _) =
  query conn "SELECT id,tag FROM tags WHERE user_id = ?" (Only uid)
