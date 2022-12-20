import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.ST.Trans
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Lazy
import           Data.Array
import           Data.Array.ST
import           Data.Char
import           Data.List
import qualified Data.Map                       as Map
import           Data.Maybe
import           Debug.Trace
import           System.Console.ANSI
import           System.IO
import           System.Random
import           Utils

-- prog, instruction pointer, relative base, inputs
data ProgState s =
  ProgState
    { ip     :: Integer
    , base   :: Integer
    , inputs :: [Int]
    }

type IntProg s = STArray s Integer Integer

type GameState s = StateT (ProgState s) (STT s IO) Int

type IntOp = (Integer -> Integer -> Integer)

type IntCond = (Integer -> Integer -> Bool)

type RowData = Map.Map Int (Int, Int)

liftIO :: IO a -> StateT (ProgState s) (STT s IO) a
liftIO = lift . lift

main :: IO ()
main = do
  input <- filter (/= '\n') <$> readFile "data/19-puzzle-input"
  let inputs = splitString (== ',') input
      intprog = map read inputs :: [Integer]
  p1 <- countTractor intprog 49
  (x, y) <- getSquare intprog 100
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show (10000 * x + y)

countTractor :: [Integer] -> Int -> IO Int
countTractor prog b = do
  s <- runSTT $ findGrid prog b
  return $ length (filter (== 1) s)

findGrid :: [Integer] -> Int -> STT s IO [Int]
findGrid prog b =
  let pos = liftM2 lister [0 .. b] [0 .. b]
   in mapM (runProgram prog) pos
  where
    lister a b = [a, b]

getSquare :: [Integer] -> Int -> IO (Int, Int)
getSquare prog b = do
  let init = Map.singleton 5 (3, 1) -- y pos, x position, length
  rows <- foldM (computeRow prog) init [6 .. 2000]
  let Just y = find (fits b rows) [6 .. 2000]
  return (fst $ rows Map.! (y + b - 1), y)

fits b rows y =
  let (s, l) = rows Map.! y
      (s', l') = rows Map.! (y + b - 1)
   in l - (s' - s) >= b

computeRow :: [Integer] -> RowData -> Int -> IO RowData
computeRow prog rd y' = do
  let (x, l) = rd Map.! (y' - 1)
  x' <- findStart prog y' x
  l' <- findL prog y' x' l
  return $ Map.insert y' (x', l') rd

findStart prog y x = do
  s <- runSTT $ runProgram prog [x, y]
  if s == 1
    then return x
    else findStart prog y (x + 1)

findL prog y start l = do
  s1 <- runSTT $ runProgram prog [start + l - 1, y]
  s2 <- runSTT $ runProgram prog [start + l, y]
  case (s1, s2) of
    (0, 0) -> findL prog y start (l - 1)
    (1, 1) -> findL prog y start (l + 1)
    (1, 0) -> return l

runProgram :: [Integer] -> [Int] -> STT s IO Int
runProgram intprog ins = do
  prog <- newArray (0, 4096) 0
  zipWithM_ (writeArray prog) [0 ..] intprog
  let initState = ProgState 0 0 ins
  -- evaluate the program
  evalStateT (runProgram' prog) initState

runProgram' :: IntProg s -> GameState s
runProgram' prog = do
  i <- gets ip
  instr <- lift $ readArray prog i
  case instr `rem` 100 of
    1  -> plusMult prog (+)
    2  -> plusMult prog (*)
    3  -> input prog
    4  -> output prog
    5  -> jump prog (/=)
    6  -> jump prog (==)
    7  -> cond prog (<)
    8  -> cond prog (==)
    9  -> adjBase prog
    99 -> return $ -1

shift offset instr = instr `div` place `rem` 10
  where
    place = 10 ^ (offset + 1)

getPmode prog offset = do
  i <- gets ip
  bp <- gets base
  lift $ shift offset <$> readArray prog i

{-# ANN readParam "Hlint: ignore Reduce duplication" #-}

readParam :: IntProg s -> Integer -> StateT (ProgState s) (STT s IO) Integer
readParam prog offset = do
  i <- gets ip
  bp <- gets base
  pmode <- getPmode prog offset
  lift $
    case pmode of
      0 -> readArray prog >=> readArray prog $ i + offset
      1 -> readArray prog (i + offset)
      2 -> readArray prog . (+ bp) =<< readArray prog (i + offset)

readParams :: IntProg s -> StateT (ProgState s) (STT s IO) (Integer, Integer)
readParams prog = liftM2 (,) (readParam prog 1) (readParam prog 2)

{-# ANN writeParam "Hlint: ignore Reduce duplication" #-}

writeParam ::
     IntProg s -> Integer -> Integer -> StateT (ProgState s) (STT s IO) ()
writeParam prog offset val = do
  i <- gets ip
  bp <- gets base
  pmode <- getPmode prog offset
  c <-
    lift $
    case pmode of
      0 -> readArray prog (i + offset)
      2 -> (+ bp) <$> readArray prog (i + offset)
  void $ lift (writeArray prog c val)

putIP i = modify (\s -> s {ip = i})

putBase b = modify (\s -> s {base = b})

putInputs i = modify (\s -> s {inputs = i})

plusMult :: IntProg s -> IntOp -> GameState s
plusMult prog op = do
  (a, b) <- readParams prog
  writeParam prog 3 $ a `op` b
  gets ip >>= putIP . (+ 4)
  runProgram' prog

input :: IntProg s -> GameState s
input prog = do
  m:ins <- gets inputs
  writeParam prog 1 (fromIntegral m)
  putInputs ins
  gets ip >>= putIP . (+ 2)
  runProgram' prog

output :: IntProg s -> GameState s
output prog = do
  c <- fromIntegral <$> readParam prog 1
  gets ip >>= putIP . (+ 2)
  return c

jump :: IntProg s -> IntCond -> GameState s
jump prog p = do
  i <- gets ip
  (a, b) <- readParams prog
  let i' =
        if p a 0
          then b
          else i + 3
  putIP i'
  runProgram' prog

cond :: IntProg s -> IntCond -> GameState s
cond prog p = do
  (a, b) <- readParams prog
  let val =
        if p a b
          then 1
          else 0
  writeParam prog 3 val
  gets ip >>= putIP . (+ 4)
  runProgram' prog

adjBase :: IntProg s -> GameState s
adjBase prog = do
  a <- readParam prog 1
  gets base >>= putBase . (+ a)
  gets ip >>= putIP . (+ 2)
  runProgram' prog
