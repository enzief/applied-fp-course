{-# LANGUAGE OverloadedStrings #-}
module Level07.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks)

import           Data.Bifunctor                     (first)
import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, FromRow,
                                                     Query (fromQuery), ToRow)
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level07.AppM                      (App, AppM (..), Env (envDB))

import           Level07.Types                     (Comment, CommentText,
                                                     DBFilePath (getDBFilePath),
                                                     Error (DBError),
                                                     FirstAppDB (FirstAppDB, dbConn),
                                                     Topic, fromDBComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)
import           Level07.Types.Topic                 (topicParser)

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: DBFilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open ( getDBFilePath fp )
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

getDBConn
  :: App Connection
getDBConn =
  AppM $ pure . pure . dbConn . envDB

runDB
  :: (a -> Either Error b)
  -> (Connection -> IO a)
  -> App b
runDB f g =
  do
    conn <- getDBConn
    AppM . const $ f <$> g conn

getComments
  :: Topic
  -> App [Comment]
getComments tp =
  AppM $ \env ->
    let
      sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
      conn = dbConn $ envDB env
    in
      traverse fromDBComment <$> Sql.query conn sql tp

addCommentToTopic
  :: Topic
  -> CommentText
  -> App ()
addCommentToTopic tp txt =
  AppM $ \env ->
    let
      sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
      conn = dbConn $ envDB env
    in do
      t <- getCurrentTime
      pure <$> Sql.execute conn sql (getTopic tp, getCommentText txt, t)

getTopics
  :: App [Topic]
getTopics =
  AppM $ \env ->
    let
      sql = "SELECT DISTINCT topic FROM comments"
      conn = dbConn $ envDB env
    in
      sequence <$> Sql.queryWith_ topicParser conn sql

deleteTopic
  :: Topic
  -> App ()
deleteTopic tp =
  AppM $ \env ->
    let
      sql = "DELETE FROM comments WHERE topic = ?"
      conn = dbConn $ envDB env
    in
      pure <$> Sql.execute conn sql [getTopic tp]

-- Go on to 'src/Level07/Core.hs' next.
