module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.Maybe
------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row] 
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku [[Nothing | _ <- [1..9]] | _ <- [1..9]]

test = Sudoku [[Nothing | _ <- [1..9]] | _ <- [1..9]]

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rows) =
    length rows == 9 &&
    all (\row -> length row == 9 && all isValidCell row) rows

isValidCell :: Cell -> Bool
isValidCell Nothing  = True
isValidCell (Just n) = n >= 1 && n <= 9


-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku rows) = and [isCellFilled cell | row <- rows, cell <- row]

isCellFilled :: Cell -> Bool
isCellFilled (Just _) = True
isCellFilled Nothing  = False

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku rows) = do
  putStrLn (unlines (map rowToString rows))
  where
    rowToString :: Row -> String
    rowToString row = map cellToChar row

    cellToChar :: Cell -> Char
    cellToChar (Just n) = head (show n)
    cellToChar Nothing  = '.'
  
  



-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku filePath = do
    contents <- readFile filePath
    let sudokuRows = map (map charToCell) (lines contents)
        sudoku = Sudoku sudokuRows
    if isSudoku sudoku
        then return (sudoku)
        else error "Not a Sudoku!"

charToCell :: Char -> Cell
charToCell '.' = Nothing
charToCell c
    | c `elem` ['1'..'9'] = Just (digitToInt c)
    | otherwise = error "Invalid character in Sudoku!" 
------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Cell)
cell = frequency [(1,justCell),(9,emptyCell)]
  where
    justCell = do
      n <- choose(1, 9)
      return(Just n)
    emptyCell = do 
      return Nothing


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = do
    rows <- vectorOf 9 (vectorOf 9 cell)
    return(Sudoku rows)

 -- hint: get to know the QuickCheck function vectorOf
 
-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sudoku = isSudoku sudoku
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1

isOkayBlock :: Block -> Bool
isOkayBlock block = all isUnique (filter isJust block)
  where
    isUnique :: Maybe Int -> Bool
    isUnique (Just n) = countOccurrences n block == 1

    countOccurrences :: Int -> Block -> Int
    countOccurrences n block = length (filter (\x -> x == Just n) block)


-- * D2

blocks :: Sudoku -> [Block]
blocks (Sudoku rows) = rows ++ columns rows ++ 3x3block rows

columns :: [Row] -> [Block]
column rows = undefined

3x3block :: [Row] -> [Block]


prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths = undefined

-- * D3

isOkay :: Sudoku -> Bool
isOkay = undefined


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks = undefined

--prop_blanks_allBlanks :: ...
--prop_blanks_allBlanks =


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = undefined

--prop_bangBangEquals_correct :: ...
--prop_bangBangEquals_correct =


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update = undefined

--prop_update_updated :: ...
--prop_update_updated =


------------------------------------------------------------------------------

-- * F1


-- * F2


-- * F3


-- * F4
