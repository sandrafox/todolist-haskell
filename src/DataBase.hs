module DataBase where

    import Database.HDBC
    import Database.HDBC.Sqlite3
    import Control.Monad(when, forM)
    import Data.List(sort)
    import Control.Monad.IO.Class
    import Types
    
    connect :: FilePath -> IO Connection
    connect fp =
        do dbh <- connectSqlite3 fp
           prepDB dbh
           cleanDB dbh
           return dbh
    
    
    prepDB :: IConnection conn => conn -> IO ()
    prepDB dbh =
        do tables <- getTables dbh
           when (not ("posts" `elem` tables)) $
               do run dbh "CREATE TABLE list (\
                           \todoId INTEGER NOT NULL PRIMARY KEY ,\
                           \description TEXT NOT NULL,\
                           \tags TEXT NOT NULL)" []
                  return ()
           commit dbh
           
    cleanDB db = do run db "delete from list" []
                    commit db

    addTodo :: IConnection conn => conn -> TodoItem -> IO ()
    addTodo db todo = do
       liftIO $ runSqlWithTransaction db "INSERT INTO list (todoId, description, tags) VALUES (?, ?, ?)" [toSql . tiIndex . getIndex todo, toSql . tiDescription . getDescription todo, toSql . tiTags todo]
       return ()

    doneTodo :: IConnection conn => conn -> Index -> IO ()
    doneTodo db index = do
       liftIO $ runWithSqlTransaction db "DELETE FROM list WHERE todoId=?" [toSql . getIndex index]
       return ()