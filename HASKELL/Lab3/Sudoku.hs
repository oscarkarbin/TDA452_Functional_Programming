module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.Maybe
import Data.List

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
    rowToString = map cellToChar

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
        then return sudoku
        else error "Not a Sudoku!"

charToCell :: Char -> Cell
charToCell '.' = Nothing
charToCell c
    | c `elem` ['1'..'9'] = Just (digitToInt c)
    | otherwise = error "Invalid character in Sudoku!" 
------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen Cell
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

-- simple test
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku 
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1
-- | Checks if a block in the Sudoku puzzle is valid.
--   A block is valid if it contains unique digits (1-9) and no duplicates.

isOkayBlock :: Block -> Bool
isOkayBlock block = all isUnique (filter isJust block)
  where
    isUnique :: Maybe Int -> Bool
    isUnique (Just n) = countOccurrences n block == 1

    countOccurrences :: Int -> Block -> Int
    countOccurrences n block = length (filter (\x -> x == Just n) block)


-- * D2
-- | Extracts all blocks (rows, columns, and 3x3 subgrids) from a Sudoku puzzle.
blocks :: Sudoku -> [Block]
blocks (Sudoku rows) = rows ++ transpose rows ++ blocks3x3 rows


-- | Extracts 3x3 subgrids from a list of rows.
blocks3x3 :: [Row] -> [Block]
blocks3x3 rows = concatMap (map concat . transpose . map (listSplit 3)) (listSplit 3 rows)

-- | Splits a list into smaller chunks of a specified size.
listSplit:: Int -> [a] -> [[a]]
listSplit _ [] = []
listSplit n xs = take n xs: listSplit n (drop n xs)
      

-- | Property test function that checks if the lengths of all blocks in a Sudoku puzzle are valid.
prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sudoku =
    length (blocks sudoku) == 3 * 9 &&
    all (\block -> length block == 9) (blocks sudoku)

-- * D3

-- | Checks if a Sudoku puzzle is valid by ensuring that all its blocks (rows, columns, and 3x3 subgrids)
--   do not contain repeated digits (1-9).
isOkay :: Sudoku -> Bool
isOkay sudoku =  all isOkayBlock (blocks sudoku)



---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

-- | Extracts all blank positions (cells with no value) from a Sudoku puzzle.
-- It iterates over each cell in the Sudoku grid, returning a list of positions where the cells are blank.
blanks :: Sudoku -> [Pos]
blanks (Sudoku rows) = [(row,col) | (row, rowValues) <- zip [0..8] rows
                                  , (col, colValues) <- zip [0..8] rowValues
                                  , isNothing colValues]

-- | Property to check that a completely blank Sudoku has exactly 81 blank cells.
prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks = length (blanks allBlankSudoku) == 81



-- * E2
-- | Updates a given list with a new value at a specified index.
-- For example, ['a','b','c','d'] updated at index 1 with 'X' results in ['a','X','c','d'].
(!!=) :: [a] -> (Int, a) -> [a]
xs !!= (i, newValue) = take i xs ++ [newValue] ++ drop (i + 1) xs

-- | Property to test the correctness of the !!= function.
-- It checks that after the update, the element at the specified index is equal to the new value.
prop_bangBangEquals_correct :: [Char] -> (Int, Char) -> Property
prop_bangBangEquals_correct xs (i, y) =
  i >= 0 && i < length xs ==>
    (xs !!= (i, y)) !! i === y


-- * E3
-- | Updates a Sudoku puzzle at a specified position (row and column) with a new cell value.
-- If the specified position is out of bounds, it returns the unchanged Sudoku.
update :: Sudoku -> Pos -> Cell -> Sudoku
update (Sudoku rows) (row, col) newVal
  | row < 0 || row >= 9 || col < 0 || col >= 9 = Sudoku rows
  | otherwise = Sudoku (rows !!= (row, (rows !! row) !!= (col, newVal)))

-- | Property to check that the update function correctly updates the cell at a specified position.
-- It validates that if the position is valid, the cell at that position contains the new value.
prop_update_updated :: Sudoku -> Pos -> Cell -> Bool
prop_update_updated sudoku (row, col) newVal
  | row < 0 || row >= 9 || col < 0 || col >= 9 = updatedSudoku == sudoku -- Here we check that no change occurs on invalid position
  | otherwise = getCell updatedSudoku (row, col) == newVal
  where
    updatedSudoku = update sudoku (row, col) newVal
    getCell (Sudoku rows) (r, c) = (rows !! r) !! c


-- * F1

-- | The main function to solve a Sudoku puzzle. It returns a solved Sudoku (if solvable) or Nothing otherwise.
-- It uses a recursive helper function 'solve'' that generates a list of all possible solutions,
-- and lazily evaluates to find the first valid solution.
solve :: Sudoku -> Maybe Sudoku
solve sudoku
  | not $ isOkay sudoku = Nothing
  | otherwise = case solve' sudoku (blanks sudoku) of
      [] -> Nothing
      (s:_) -> Just s

-- | Recursive helper function for 'solve'. It takes a Sudoku and a list of blank positions,
-- and returns a list of all possible solutions.
solve' :: Sudoku -> [Pos] -> [Sudoku]
solve' sudoku []
  | isOkay sudoku = [sudoku]
  | otherwise = []
solve' sudoku (pos:poss)
  | not $ isOkay sudoku = []
  | otherwise = concatMap (`solve'` poss) [update sudoku pos (Just val) | val <- [1..9]]


-- * F2
-- | Reads a Sudoku puzzle from a file, solves it, and prints the solution or a message if there's no solution.
readAndSolve :: FilePath -> IO()
readAndSolve inputfile = do
  sud <- readSudoku inputfile
  maybe (putStrLn "(No solution)") printSudoku (solve sud)


-- * F3
-- | Checks if one Sudoku is a solution of another. It verifies that the first Sudoku (solution) is valid,
-- completely filled, and all its filled cells match with the cells in the second Sudoku (candidate).
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf solution candidate =
    isSudoku solution &&
    isSudoku candidate &&
    isFilled solution &&
    isOkay solution &&
    candidate `contains` solution

-- | Helper function for 'isSolutionOf'. It checks if all filled cells in the second Sudoku (candidate)
-- match with the corresponding cells in the first Sudoku (solution).
contains :: Sudoku -> Sudoku -> Bool
contains (Sudoku rows1) (Sudoku rows2) =
    all (\(row1, row2) -> all cellMatches (zip row1 row2)) (zip rows1 rows2)
  where
    cellMatches :: (Cell, Cell) -> Bool
    cellMatches (cell1, cell2) =
        case (cell1, cell2) of
            (Nothing, _)       -> True
            (Just n1, Just n2) -> n1 == n2
            _                  -> False

-- * F4
-- | Property to ensure the soundness of the 'solve' function.
-- It verifies that if a solution is found for a Sudoku, it is indeed a valid solution of the original puzzle.
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sudoku =
  isJust (solve sudoku) ==>
    fromJust (solve sudoku) `isSolutionOf` sudoku

-- | A utility function to run QuickCheck tests with a reduced number of checks for efficiency.
fewerChecks prop = quickCheckWith stdArgs{maxSuccess=30 } prop