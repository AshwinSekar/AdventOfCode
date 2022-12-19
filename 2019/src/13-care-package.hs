import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.ST.Trans
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.Array.ST
import Data.List
import Data.Maybe
import Debug.Trace
import System.Console.ANSI
import System.IO
import Utils

-- prog, instruction pointer, relative base, inputs
data ProgState s = ProgState
  { ip :: Integer,
    base :: Integer,
    outputs :: [Integer],
    key :: MVar Char,
    inputF :: InputF s,
    ballx :: Integer,
    paddlex :: Integer
  }

type IntProg s = STArray s Integer Integer

type GameState s = StateT (ProgState s) (STT s IO) ()

type IntOp = (Integer -> Integer -> Integer)

type IntCond = (Integer -> Integer -> Bool)

type InputF s = StateT (ProgState s) (STT s IO) Integer

liftIO :: IO a -> StateT (ProgState s) (STT s IO) a
liftIO = lift . lift

main :: IO ()
main = do
  input <- filter (/= '\n') <$> readFile "data/13-puzzle-input"
  let inputs = splitString (== ',') input
      intprog = map read inputs :: [Integer]
  key <- newEmptyMVar
  initBoard <- runSTT $ runProgram intprog key getKey
  clearScreen >> setCursorPosition 0 0
  putStrLn $ "Part 1: " ++ show (findBlocks initBoard)
  playGame (2 : drop 1 intprog) key False

playGame :: [Integer] -> MVar Char -> Bool -> IO ()
playGame prog key manual = do
  putStrLn "Part 2 - Score: "
  hideCursor
  if manual
    then setupKeypress >> forkIO getKeypress
    else myThreadId
  runSTT $ runProgram prog key (if manual then getKey else getAi)
  setCursorPosition 40 0
  putStrLn "Game Over"
  where
    getKeypress = getChar >>= putMVar key >> getKeypress
    setupKeypress = hSetBuffering stdin NoBuffering >> hSetEcho stdin False

displayBoard = gets outputs >>= displayBoard'

displayBoard' :: [Integer] -> GameState s
displayBoard' [] = return ()
displayBoard' (s : 0 : (-1) : xs) = do
  liftIO $ setCursorPosition 1 17
  liftIO $ putStr (show s)
  displayBoard' xs
displayBoard' (t : y : x : xs) = do
  tile <- case t of
    0 -> return ' '
    1 -> return '▓'
    2 -> return '■'
    3 -> putPaddleX x >> return '='
    4 -> putBallX x >> return 'o'
  liftIO $ setCursorPosition (fromIntegral y + 2) (fromIntegral x)
  liftIO $ putChar tile
  displayBoard' xs

findBlocks :: [Integer] -> Int
findBlocks board = length $ filter (== 2) tileIds
  where
    tileIds = map fst $ filter third (zip board [1 ..])
    third = (== 1) . (`rem` 3) . snd

runProgram :: [Integer] -> MVar Char -> InputF s -> STT s IO [Integer]
runProgram intprog key input = do
  prog <- newArray (0, 4096) 0
  zipWithM_ (writeArray prog) [0 ..] intprog
  let initState = ProgState 0 0 [] key input 0 0
  -- evaluate the program
  outputs <$> execStateT (runProgram' prog >> displayBoard) initState

runProgram' :: IntProg s -> GameState s
runProgram' prog = do
  i <- gets ip
  instr <- lift $ readArray prog i
  case instr `rem` 100 of
    1 -> plusMult prog (+)
    2 -> plusMult prog (*)
    3 -> input prog
    4 -> output prog
    5 -> jump prog (/=)
    6 -> jump prog (==)
    7 -> cond prog (<)
    8 -> cond prog (==)
    9 -> adjBase prog
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
  lift $ case pmode of
    0 -> readArray prog >=> readArray prog $ i + offset
    1 -> readArray prog (i + offset)
    2 -> readArray prog =<< ((+ bp) <$> readArray prog (i + offset))

readParams :: IntProg s -> StateT (ProgState s) (STT s IO) (Integer, Integer)
readParams prog = liftM2 (,) (readParam prog 1) (readParam prog 2)

{-# ANN writeParam "Hlint: ignore Reduce duplication" #-}
writeParam :: IntProg s -> Integer -> Integer -> StateT (ProgState s) (STT s IO) ()
writeParam prog offset val = do
  i <- gets ip
  bp <- gets base
  pmode <- getPmode prog offset
  c <- lift $ case pmode of
    0 -> readArray prog (i + offset)
    2 -> (+ bp) <$> readArray prog (i + offset)
  void $ lift (writeArray prog c val)

putIP i = modify (\s -> s {ip = i})

putBase b = modify (\s -> s {base = b})

putOutputs o = modify (\s -> s {outputs = o})

putPaddleX x = modify (\s -> s {paddlex = x})

putBallX x = modify (\s -> s {ballx = x})

plusMult :: IntProg s -> IntOp -> GameState s
plusMult prog op = do
  (a, b) <- readParams prog
  writeParam prog 3 $ a `op` b
  gets ip >>= putIP . (+ 4)
  runProgram' prog

getKey :: InputF s
getKey = do
  -- get input
  k <- wait
  (liftIO . threadDelay) 500000
  return $ case k of
    'a' -> -1
    'd' -> 1
    _ -> 0

getAi :: InputF s
getAi = do
  paddleX <- gets paddlex
  ballX <- gets ballx
  (liftIO . threadDelay) 50000
  return $ case compare paddleX ballX of
    LT -> 1
    EQ -> 0
    GT -> -1

input :: IntProg s -> GameState s
input prog = do
  -- display board
  displayBoard >> putOutputs []
  liftIO $ hFlush stdout
  writeParam prog 1 =<< (join . gets) inputF
  gets ip >>= putIP . (+ 2)
  runProgram' prog

wait = do
  k <- (liftIO . tryTakeMVar) =<< gets key
  maybe ((liftIO . threadDelay) 500000 >> wait) return k

output :: IntProg s -> GameState s
output prog = do
  a <- readParam prog 1
  gets ip >>= putIP . (+ 2)
  gets outputs >>= putOutputs . (a :)
  runProgram' prog

jump :: IntProg s -> IntCond -> GameState s
jump prog p = do
  i <- gets ip
  (a, b) <- readParams prog
  let i' = if p a 0 then b else i + 3
  putIP i'
  runProgram' prog

cond :: IntProg s -> IntCond -> GameState s
cond prog p = do
  (a, b) <- readParams prog
  let val = if p a b then 1 else 0
  writeParam prog 3 val
  gets ip >>= putIP . (+ 4)
  runProgram' prog

adjBase :: IntProg s -> GameState s
adjBase prog = do
  a <- readParam prog 1
  gets base >>= putBase . (+ a)
  gets ip >>= putIP . (+ 2)
  runProgram' prog
