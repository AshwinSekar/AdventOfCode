import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.ST.Trans
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class

import Data.Array.ST
import Data.List
import Data.Maybe

import Debug.Trace

import System.IO
import System.Console.ANSI
import System.Random

import Utils

-- prog, instruction pointer, relative base, inputs
data ProgState s = ProgState { ip :: Integer,
                               base :: Integer,
                               movement :: Integer,
                               status :: Integer,
                               pos :: (Int, Int),
                               key :: MVar Char,
                               inputF :: InputF s
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
  input <- filter (/= '\n') <$> readFile "data/15-puzzle-input"
  k <- newEmptyMVar
  let inputs = splitString (== ',') input
      intprog = map read inputs :: [Integer]
  runDroid intprog k


runDroid :: [Integer] -> MVar Char -> IO ()
runDroid prog k = do
  clearScreen
  hideCursor
  setupKeypress -- >> forkIO getKeypress
  setCursorPosition 30 60 >> putChar '®'
  hFlush stdout
  -- runSTT $ runProgram prog k getKey
  runSTT $ runProgram prog k getAi
  -- Mark the start and move the newline to the bottom
  setCursorPosition 30 60 >> putChar 'x' >> setCursorPosition 70 0
  where getKeypress = getChar >>= putMVar k >> getKeypress
        setupKeypress = hSetBuffering stdin NoBuffering >> hSetEcho stdin False

displayDroid = do
  (posx, posy) <- gets pos
  liftIO $ setCursorPosition posy posx >> putChar '·'
  stat <- gets status
  mov <- gets movement
  let (posx', posy') = case mov of
                         1 -> (posx, posy - 1)
                         2 -> (posx, posy + 1)
                         3 -> (posx - 1, posy)
                         4 -> (posx + 1, posy)
  liftIO $ setCursorPosition posy' posx'
  case stat of
    0 -> liftIO $ putChar '█' >> setCursorPosition posy posx >> putChar '®'
    1 -> liftIO (putChar '®') >> putPos (posx', posy')
    2 -> liftIO (putChar '§') >> putPos (posx', posy')
  liftIO $ hFlush stdout


runProgram :: [Integer] -> MVar Char -> InputF s -> STT s IO ()
runProgram intprog k input = do
  prog <- newArray (0, 4096) 0
  zipWithM_ (writeArray prog) [0..] intprog
  let initState = ProgState 0 0 0 0 (60, 30) k input
  -- evaluate the program
  evalStateT (runProgram' prog) initState


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
  where place = 10 ^ (offset + 1)

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
putStatus o = modify (\s -> s {status = o})
putPos p = modify (\s -> s {pos = p})
putMov m = modify (\s -> s {movement = m})

plusMult :: IntProg s -> IntOp -> GameState s
plusMult prog op = do
  (a, b) <- readParams prog
  writeParam prog 3 $ a `op` b
  gets ip >>= putIP . (+4)
  runProgram' prog

getKey :: InputF s
getKey = do
  -- get input
  k <- wait
  case k of
    'w' -> return 1
    'a' -> return 3
    's' -> return 2
    'd' -> return 4
    _ -> getKey

getAi :: InputF s
getAi = liftIO $ randomRIO (1, 4)

input :: IntProg s -> GameState s
input prog = do
  mov <- (join . gets) inputF
  writeParam prog 1 mov
  putMov mov
  gets ip >>= putIP . (+2)
  runProgram' prog

wait =  do
  k <- (liftIO . tryTakeMVar) =<< gets key
  maybe ((liftIO . threadDelay) 5000 >> wait) return k

output :: IntProg s -> GameState s
output prog = do
  status <- readParam prog 1
  putStatus status
  displayDroid
  gets ip >>= putIP . (+2)
  if status == 2
    then return ()
    else runProgram' prog

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
  gets ip >>= putIP . (+4)
  runProgram' prog

adjBase :: IntProg s -> GameState s
adjBase prog = do
  a <- readParam prog 1
  gets base >>= putBase . (+a)
  gets ip >>= putIP . (+2)
  runProgram' prog
