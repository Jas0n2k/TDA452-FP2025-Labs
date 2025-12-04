module Sudoku where

import Test.QuickCheck
import Data.List
import Data.Char
import Data.Maybe

------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row] 
 deriving ( Show, Eq )

-- Extracts the rows from the sudoku
rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
  Sudoku
    [[j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ],
     [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ],
     [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ],
     [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8],
     [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9],
     [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ],
     [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ],
     [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ],
     [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]]
  where
    n = Nothing
    j = Just


-- Example usage: create 9x9 grid filled with 5s
all_filled :: Sudoku
all_filled =
    Sudoku (replicate 9 (replicate 9 (j 5)))

  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
-- Use `replicate` twice two create a 9x9 grid of Nothing
allBlankSudoku = Sudoku (replicate 9 (replicate 9 n))
  where n = Nothing

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rows) = (all isValidRow rows) && length rows == 9
  where
    isValidCell :: Cell -> Bool
    -- Checks if the given non-nothing cell is valid
    isValidCell (Just i) = i >= 1 && i <= 9
    isValidCell Nothing  = True 

    -- Checks if all cells in a row are valid and the length of the row is 9
    isValidRow :: Row -> Bool
    isValidRow r = (all isValidCell r) && length r == 9

-- * A3

-- | Returns true if the given cell is filled with a number 
isFilledCell :: Cell -> Bool
isFilledCell (Just _) = True
isFilledCell _        = False

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku rows) = all isFilledRow rows
  where 
    isFilledRow :: Row -> Bool
    isFilledRow r = all isFilledCell r

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku rows) = putStrLn (intercalate "\n" (map printRow rows))
  where
    printCell :: Cell -> String
    printCell (Just n) = show n
    printCell Nothing  = "."

    printRow :: Row -> String
    printRow r = intercalate "" (map printCell r)

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do
  d <- readFile file
  let rowStrings = lines d
  
  -- Iterates over list of String and maps it to readCell
  let rows = map (\rowString -> (map readCell rowString)) rowStrings
  let sudoku = Sudoku rows
  return (validate sudoku)

  where
    readCell :: Char -> Cell
    readCell c 
      | c == '.'  = Nothing
      | isDigit c = Just (digitToInt c) -- construct `Just c` only when c is a digit
      | otherwise = error "Invalid character in sudoku" 
    -- Takes a sudoku and returns it if it's valid, otherwise throwing an exception
    validate :: Sudoku -> Sudoku
    validate s 
      | isSudoku s = s
      | otherwise = error "Invalid shape of sudoku"

------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Cell)
cell = frequency [(8, rNothing), (2, rNumeric)]
  where
    rNothing = return Nothing
    rNumeric = Just <$> choose(1,9) -- returns a `Just` wrapped number


-- * C2

-- | an instance for generating Arbitrary 9x9 Sudokus
instance Arbitrary Sudoku where
  arbitrary = Sudoku <$> vectorOf 9 (vectorOf 9 cell)
 
-- * C3

-- | Property for checking if a sudoku is valid
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku s = isSudoku s
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1

-- | Returns if the block is okay, i.e there are no duplicate numbers in the block 
isOkayBlock :: Block -> Bool
isOkayBlock block = length nums == length (nub nums)
  where nums =[n | Just n <- block ]

-- * D2

-- | Extracts the blocks out of the given sudoku
blocks :: Sudoku -> [Block]
blocks (Sudoku rows) = rowBlock ++ colBlock ++ sqrBlock
  where rowBlock   = rows
        colBlock   = transpose rows
        sqrBlock   = concat (map (\band -> cutByCol band) (cutIntoBand rows))
          where 
              cutIntoBand :: [Row] -> [[Row]]
              cutIntoBand s = [upperBand, midBand, lowerBand]
                where upperBand = take 3 s
                      midBand   = take 3 (drop 3 s)
                      lowerBand = drop 6 s
                -- Processes three-row band by dividing it into 3 three-column blocks
              cutByCol :: [Row] -> [Block] 
              cutByCol band = [leftBlock, midBlock, rightBlock]
                where leftBlock  = concat [ take 3 r | r <- band]
                      midBlock   = concat [ take 3 (drop 3 r) | r <- band]
                      rightBlock = concat [ drop 6 r | r <- band]
        

-- | Verifies that there are 3x9 blocks in the sudoku, and that each block has 9 cells
prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sudoku = length sudokuBlocks == 27 && all (\b -> length b == 9) sudokuBlocks
    where
      sudokuBlocks = blocks sudoku

-- * D3

-- | Validates a sudoku by checking that all blocks are valid (i.e contains no duplicate numbers)
isOkay :: Sudoku -> Bool
isOkay s = all (\b -> isOkayBlock b) sudokuBlocks
  where
    sudokuBlocks = blocks s


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1: Find all blank positions in a sudoku

blanks :: Sudoku -> [Pos]
-- | zip [0..] rows -> [(0,row0), (1,row1), ..., (8,row8)] (row index paired with row)
-- | zip [0..] row -> [(0,cell0), (1,cell1), ..., (8,cell8)] (column index paired with cell)
-- | Guard isNothing cell to filter only blank cells
blanks (Sudoku rows) = [ (r, c) | (r, row) <- zip [0..] rows, (c, cell) <- zip [0..] row, isNothing cell ]

prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks = (blanks allBlankSudoku) == [(r, c) | r <- [0..8], c <- [0..8] ]


-- * E2: Update an element in a list at a given index

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = before ++ y:after
  where 
    (before, _:after) = splitAt i xs

-- Use generated lists and indices to verify
prop_bangBangEquals_correct :: [Int] -> Int -> Property
prop_bangBangEquals_correct list v = 
    not (null list) ==> 
    forAll (choose (0, length list - 1)) $ \i -> 
        let updatedList = list !!= (i, v)
        in (updatedList !! i) == v && length list == length updatedList

-- * E3: Update a cell in a sudoku at a given position

update :: Sudoku -> Pos -> Cell -> Sudoku
update (Sudoku sudoku) (row, col) cell = (Sudoku (sudoku !!= (row, ((sudoku !! row) !!= (col, cell)))))

-- Use generated valid positions and valid cells to verify
prop_update_updated :: Sudoku -> Property
prop_update_updated sudoku = forAll validPositions $ \(row, col) -> 
    forAll cell $ \c ->
        let (Sudoku updated) = update sudoku (row, col) c
        in (updated !! row) !! col == c
  where
    validPositions = do
      row <- choose (0, 8)
      col <- choose (0, 8)
      return (row, col)




------------------------------------------------------------------------------

-- * F1: Take in a sudoku and attempt to solve it. 
-- | Only return first found solution.
-- | Return Nothing if unable to.
solve :: Sudoku -> Maybe Sudoku
solve sudoku 
    | (solutions == [])   =   Nothing
    | otherwise           =   Just (head solutions) -- Return the first solution found
  where
    solutions = solve' sudoku (blanks sudoku)

    solve' :: Sudoku -> [(Int, Int)] -> [Sudoku]
    solve' sud [] -- Case: blanks is empty
      | isOkay sud = [sud] -- Found a valid solution
      | otherwise  = [] -- No solution found
    solve' sud (blank:rest)
      | not (isOkay sud) = [] -- Early exit if the sudoku is already invalid
      | otherwise = concatMap tryFilling [1..9] -- Try filling the blank with numbers from 1 to 9
      where
        tryFilling :: Int -> [Sudoku]
        tryFilling num = solve' (update sud blank (Just num)) rest

-- * F2
-- | Reads a sudoku from a file, tries to solve it and prints the solution (or lack of one)
readAndSolve :: FilePath -> IO ()
readAndSolve file = do
  sudoku <- readSudoku file
  let solution = solve sudoku
  case solution of
     Just solved -> printSudoku solved
     Nothing -> putStrLn $ "(no solution)"

-- * F3
-- | Verifies that the first argument (solution) is in fact a solution of the original sudoku
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf solution sudoku = isFilled solution && isOkay solution && all (\(r,c,v) -> solution_rows !! r !! c == v) filled_cells
  where
    filled_cells = [ (r, c, sudoku_rows !! r !! c) | 
        (r, row) <- zip [0..] sudoku_rows, 
        (c, cell) <- zip [0..] row, isJust cell ] -- All filled cells in original sudoku

    solution_rows = rows solution
    sudoku_rows = rows sudoku


-- * F4
-- | QuickCheck property validating that a solved sudoku is in fact a correct solution of the given sudoku
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sudoku = isJust solved ==> fromJust solved `isSolutionOf` sudoku
  where solved = solve sudoku
