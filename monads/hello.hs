

askForName :: IO ()
askForName = putStrLn "What is your name?"

sayHello :: String -> String
sayHello name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName = askForName >>
            getLine >>= 
            (\name ->
                return (sayHello name)) >>=
            putStrLn

helloNameDo :: IO ()
helloNameDo = do 
    askForName
    name <- getLine
    putStrLn $ sayHello name
