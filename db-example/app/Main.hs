module Main where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time

-- DATA MODEL
data Tool = Tool {
      toolId :: Int
    , name :: String
    , description :: String
    , lastReturned :: Day
    , timesBorrowed :: Int
    } 

instance Show Tool where
    show tool = mconcat [ show $ toolId tool
                        , ".) "
                        , name tool
                        , "\n description: "
                        , description tool
                        , "\n last returned: "
                        , show $ lastReturned tool
                        , "\n timesBorrowed: "
                        , show $ timesBorrowed tool
                        , "\n"]

instance FromRow Tool where
    fromRow = Tool  <$> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field

data User = User {
      userId :: Int
    , userName :: String
    } 

instance Show User where
    show user = mconcat [ show $ userId user, ".) ", userName user ]

instance FromRow User where
    fromRow = User  <$> field
                    <*> field

-- ENTRY POINT
main :: IO ()
main = do
    print "Enter command"
    command <- getLine
    performCommand command

-- FUNCTIONS
performCommand :: String -> IO ()
performCommand command
    | command == "users" = printUsers >> main
    | command == "tools" = printTools >> main
    | command == "adduser" = promptAndAddUser >> main
    | command == "checkout" = promptAndCheckout >> main
    | command == "checkin" = promptAndCheckIn >> main
    | command == "in" = printAvailableTools >> main
    | command == "out" = printCheckedOutTools >> main
    | command == "quit" = print "Bye!" 
    | otherwise = print "Sorry! command not found" >> main

addUser :: String -> IO ()
addUser userName = do
    withConn "tools.db" (\conn -> do
        execute conn "INSERT INTO users (username) VALUES (?)" (Only userName)
        print $ "added user: " ++ userName)

checkout :: Int -> Int -> IO ()
checkout userId toolId = withConn "tools.db" $ 
                            \conn -> do
                                execute conn 
                                    "INSERT INTO checkout (user_id, tool_id) VALUES (?,?)"
                                    (userId, toolId)
                                print $ "user " ++ show userId ++ "has checked out tool " ++ show toolId

printUsers :: IO ()
printUsers = withConn "tools.db" $
                \conn -> do
                    resp <- query_ conn "SELECT * FROM users;" :: IO [User]
                    mapM_ print resp

printToolQuery :: Query -> IO ()
printToolQuery q = withConn "tools.db" $
                    \conn -> do
                        resp <- query_ conn q :: IO [Tool]
                        mapM_ print resp

printTools :: IO ()
printTools = printToolQuery "SELECT * FROM tools;"

printAvailableTools :: IO ()
printAvailableTools = printToolQuery $ mconcat [ "SELECT * FROM tools "
                                               , "WHERE id NOT IN "
                                               , "(SELECT tool_id FROM checkedout);" ] 

printCheckedOutTools :: IO ()
printCheckedOutTools = printToolQuery $ mconcat [ "SELECT * FROM tools "
                                                , "WHERE id IN "
                                                , "(SELECT tool_id FROM checkedout);" ]

selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
    resp <- query conn "SELECT * FROM tools WHERE id = (?)" (Only toolId) :: IO [Tool]
    return $ firstOrNothing resp

firstOrNothing :: [Tool] -> Maybe Tool
firstOrNothing [] = Nothing
firstOrNothing (x:_) = Just x

updateTool :: Tool -> Day -> Tool
updateTool tool date = tool { lastReturned = date, timesBorrowed = 1 + timesBorrowed tool }

updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = print "id not found"
updateOrWarn (Just tool) = withConn "tools.db" $
                                \conn -> do
                                    let q = mconcat [ "UPDATE tools SET "
                                                    , " lastReturned = ?, "
                                                    , " timesBorrowed = ? "
                                                    , "WHERE id = ?;" ]
                                    execute conn q (lastReturned tool, timesBorrowed tool, toolId tool)
                                    print "tool updated"

updateToolTable :: Int -> IO ()
updateToolTable toolId = withConn "tools.db" $
                            \conn -> do
                                tool <- selectTool conn toolId
                                currentDay <- utctDay <$> getCurrentTime
                                let updatedTool = updateTool <$> tool <*> pure currentDay
                                updateOrWarn updatedTool

checkInTool :: Int -> IO ()
checkInTool toolId = withConn "tools.db" $
                        \conn -> do
                            execute conn "DELETE FROM checkedout WHERE tool_id = (?);" (Only toolId)

checkInAndUpdate :: Int -> IO ()
checkInAndUpdate toolId = do
    checkInTool toolId
    updateToolTable toolId

promptAndAddUser :: IO ()
promptAndAddUser = do
    print "Enter new user name"
    name <- getLine
    addUser name

promptAndCheckout :: IO ()
promptAndCheckout = do
    print "Enter the id of the user"
    userId <- pure read <*> getLine
    print "Enter the id of the tool"
    toolId <- pure read <*> getLine
    checkout userId toolId

promptAndCheckIn :: IO ()
promptAndCheckIn = do
    print "Enter the id of the tool"
    toolId <- pure read <*> getLine
    checkInAndUpdate toolId

withConn :: String -> (Connection -> IO ()) -> IO()
withConn dbName action = do
    conn <- open dbName
    action conn
    close conn
