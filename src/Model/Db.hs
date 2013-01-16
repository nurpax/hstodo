{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Model.Db (
    createTables
  , newTag
  , addTag
  , removeTag
  , saveTodo
  , listTodos
  ) where

import           Control.Applicative
import           Control.Monad
import qualified Data.Text as T
import           Database.SQLite.Simple

import           Model.Types

instance FromRow Tag where
  fromRow = Tag <$> field <*> field

instance FromRow Todo where
  fromRow = Todo <$> field <*> field <*> field <*> pure []

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
                , "id INTEGER PRIMARY KEY, "
                , "user_id INTEGER NOT NULL, "
                , "saved_on TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL, "
                , "text TEXT, "
                , "done BOOLEAN)"])
    execute_ conn
      (Query $
       T.concat [ "CREATE TABLE todo_tag_map ("
                , "todo_id INTEGER, "
                , "tag_id INTEGER)"
                ])

      -- TODO multi user schema!!
    execute_ conn
      (Query $
       T.concat [ "CREATE TABLE tags ("
                , "id INTEGER PRIMARY KEY, "
                , "tag TEXT UNIQUE)"
                ])
    execute_ conn "COMMIT"

-- | Save or update a todo
newTag :: Connection -> T.Text -> IO Tag
newTag conn t = do
  execute conn "INSERT INTO tags (tag) VALUES (?)" (Only t)
  rowId <- lastInsertRowId conn
  return $ Tag rowId t

listTodoTags :: Connection -> Todo -> IO [Tag]
listTodoTags conn todo =
  query conn
    (Query $
       T.concat [ "SELECT tags.id,tags.tag FROM tags, todo_tag_map "
                , "WHERE todo_tag_map.todo_id = ? AND todo_tag_map.tag_id = tags.id"
                ])
    (Only (todoId todo))

-- | Retrieve a user's list of comments
listTodos :: Connection -> User -> IO [Todo]
listTodos conn (User uid _) = do
  todos <- query conn "SELECT id,text,done FROM todos WHERE user_id = ?" (Only uid)
  mapM queryTags todos
  where
    queryTags todo = do
      tags <- listTodoTags conn todo
      return $ todo { todoTags = tags }

-- | Save or update a todo
saveTodo :: Connection -> User -> Todo -> IO Todo
saveTodo conn (User uid _) t =
  maybe newTodo updateTodo (todoId t)
  where
    newTodo = do
      execute conn "INSERT INTO todos (user_id,text,done) VALUES (?,?,?)"
        (uid, todoText t, todoDone t)
      rowId <- lastInsertRowId conn
      return $ t { todoId = Just rowId }

    updateTodo tid = do
      execute conn "UPDATE todos SET text = ?, done = ? WHERE (user_id = ? AND id = ?)"
        (todoText t, todoDone t, uid, tid)
      return t

hasTag :: Connection -> Todo -> Tag -> IO Bool
hasTag conn todo tag = do
  [Only nRows] <-
    query conn "SELECT count(*) FROM todo_tag_map WHERE todo_id = ? AND tag_id = ?"
      (todoId todo, tagId tag) :: IO [Only Int]
  return $ nRows /= 0


addTag :: Connection -> Todo -> Tag -> IO Todo
addTag conn todo tag = do
  -- TODO how to handle dupe inserts here? just check the local todo tag list?
  tagAlreadySet <- hasTag conn todo tag
  unless tagAlreadySet $
    execute conn "INSERT INTO todo_tag_map (todo_id, tag_id) VALUES (?,?)"
      (todoId todo, tagId tag)
  newTags <- listTodoTags conn todo
  return $ todo { todoTags = newTags }

removeTag :: Connection -> Todo -> Tag -> IO Todo
removeTag conn todo tag = do
  execute conn "DELETE FROM todo_tag_map WHERE todo_id = ? and tag_id = ?"
    (todoId todo, tagId tag)
  newTags <- listTodoTags conn todo
  return $ todo { todoTags = newTags }
