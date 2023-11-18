--3A(1) Intro to IO - the Haskell way...
{-
In haskell a function is a function in a sense thata given the same input it will ALWAYS give the same answer.
Haskell is pure functional language.
-}
--3A(2) Basic IO operations
-- Small examples of IO
-- IO is the type of computations. is the type of commands that will interact with the operating system
-- There is a special type for the things in haskell that interact with the world. 

--3A (3) IO with results - do notation

-- writeFile "ex2.txt" (readFile "ex1.txt")     This wont work since a IO String is not a string
-- we need to build a copyfile functions
copyFile :: FilePath -> FilePath -> IO ()
copyFile fromFile toFile = do 
    
    c <- readFile fromFile 
        
    writeFile toFile c     -- The type of the last line in a "do block" will be the type of the result

longest :: IO String
longest = do
    wlist <- readFile "ex1.txt"
    return (long wlist)
    where long :: String -> String
          long = undefined
-- vae <- expr
-- var :: a expr :: IO a

