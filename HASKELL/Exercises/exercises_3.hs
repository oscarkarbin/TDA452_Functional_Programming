import Text.Read (readMaybe)

-- 1 Explain the error in the following examples:

{-
readFileSample1 filename = do 
  contents = readFile filename
  return (take 80 contents)

  Here the error is obvious we can't use "=" to extract the String from the IO String function "readFile filename". 
  We need to use the "<-" operator.
-}

{-
readFileSample2 filename = do 
    return (take 80 (readFile filename))

readFile filename produces an IO String. However, take 80 wont work on IO String. We need to extract the String 
from the IO String by doing:

contents <- readFile filename
return (take 80 contents)
-}

{-
readFileInteractive :: IO String
readFileInteractive = do 
  putStrLn "Type a filename"
  filename <- getLine
  return (readFile filename)

The return is causing the error here since "readFile filename" is already an IO String which is the wanted type. 
The last line of a do block will define the type of the WHOLE block.
-}


-- 2

{-
2. Suppose you have the following functions:

readAllFiles :: IO [(FilePath,String)] 
zipFiles     :: [(FilePath,String)] -> String
readAllFiles is an instruction to produce a list of all the files in the current directory together with their contents. 
zipFiles is a function that takes such a list and “zips” it (i.e. compresses it) into a single string.

Using these, write an instruction 
backupDirectory :: IO () which creates a backup of the contents of a whole directory in a file called “backup.zip”
-}


{-
backupDirectory :: IO ()
backupDirectory = do
    all_files <- readAllFiles
    writeFile "backup.zip" (zipFiles all_files)
-}

-- 3
{-
3. Write an instruction kidInCar :: IO () which repeatedly asks the user “Are we there yet?”, reads the reply, 
and only stops asking when the answer is “yes”. (Use standard functions putStrLn and getLine).
-}

kidInCar :: IO()
kidInCar = do
    putStrLn "Are we there yet?"
    input <- getLine
    case input of
        "yes" -> return()
        _     -> kidInCar

    -- if input == "yes"
    --     then return ()
    --     else kidInCar


-- 4
{-
4. Define a function sumAll :: IO Int 
which reads positive integers from the input until a zero or a non-number is input, 
at which point it returns the sum of all the numbers input so far.  A possible interaction might look like this: 

ghci> sumAll
 Next number (anything else to quit): 2
  Next number (anything else to quit): 3
  Next number (anything else to quit): 100
  Next number (anything else to quit): done
105
ghci> 
-}

sumAll :: IO Int
sumAll = do
    sumAllHelper 0

sumAllHelper :: Int -> IO Int
sumAllHelper sum = do
    putStrLn "Next number (anything else to quit):"
    input <- getLine
    case readMaybe input :: Maybe Int of 
        Nothing -> return (sum)
        Just n  -> sumAllHelper (sum + n)
    