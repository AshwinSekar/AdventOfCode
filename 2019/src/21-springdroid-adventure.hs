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

type GameState s = StateT (ProgState s) (STT s IO) ()

type IntOp = (Integer -> Integer -> Integer)

type IntCond = (Integer -> Integer -> Bool)

liftIO :: IO a -> StateT (ProgState s) (STT s IO) a
liftIO = lift . lift

main :: IO ()
main = do
  input <- filter (/= '\n') <$> readFile "data/21-puzzle-input"
  let inputs = splitString (== ',') input
      intprog = map read inputs :: [Integer]
  runSTT $ runProgram intprog
  hFlush stdout

runProgram :: [Integer] -> STT s IO ()
runProgram intprog = do
  prog <- newArray (0, 4096) 0
  zipWithM_ (writeArray prog) [0 ..] intprog
  let initState = ProgState 0 0 []
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
    99 -> return ()

shift offset instr = (instr `div` place) `rem` 10
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
      2 -> readArray prog =<< ((+ bp) <$> readArray prog (i + offset))

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

getInput :: [Int] -> StateT (ProgState s) (STT s IO) [Int]
getInput (x:xs) = return $ x : xs
getInput [] = do
  inputs <- liftIO $ readLines []
  let withNLine = map (\s -> s ++ [chr 10]) inputs
      ascii = concatMap (map ord) withNLine
  trace (show ascii) $ putInputs ascii
  return ascii

input :: IntProg s -> GameState s
input prog = do
  m:ins <- getInput =<< gets inputs
  writeParam prog 1 (fromIntegral m)
  putInputs ins
  gets ip >>= putIP . (+ 2)
  runProgram' prog

output :: IntProg s -> GameState s
output prog = do
  c <- fromIntegral <$> readParam prog 1
  if c <= 128
    then liftIO $ putChar (chr c)
    else liftIO $ putStrLn ("Total hull damage: " ++ show c)
  gets ip >>= putIP . (+ 2)
  runProgram' prog

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
