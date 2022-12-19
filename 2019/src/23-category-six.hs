import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.ST.Trans
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Lazy
import           Data.Array
import           Data.Array.ST
import           Data.Char
import           Data.Function
import           Data.Functor
import           Data.List
import qualified Data.Map                       as Map
import           Data.Maybe
import           Data.Sequence                  (Seq (..), (|>))
import qualified Data.Sequence                  as Seq
import           Debug.Trace
import           System.Console.ANSI
import           System.IO
import           System.Random
import           Utils

-- prog, instruction pointer, relative base, inputs
data Comp s =
  Comp
    { ip      :: Integer
    , base    :: Integer
    , prog    :: IntProg s
    , inputs  :: Seq.Seq Integer
    , outputs :: [Integer]
    }

data Network s =
  Network
    { idleCnt :: Integer
    , pkt     :: Maybe (Integer, Integer)
    , lastPkt :: Maybe (Integer, Integer)
    , comps   :: Map.Map Integer (Comp s)
    }

type NetworkState s a = StateT (Network s) (STT s IO) a

type IntProg s = STArray s Integer Integer

type IntOp = (Integer -> Integer -> Integer)

type IntCond = (Integer -> Integer -> Bool)

liftIO :: IO a -> NetworkState s a
liftIO = lift . lift

getsC :: Integer -> (Comp s -> a) -> NetworkState s a
getsC c f = gets comps <&> (Map.! c) <&> f

adjC :: Integer -> (Comp s -> Comp s) -> NetworkState s ()
adjC c f = modify (\s -> s {comps = Map.adjust f c (comps s)})

putIP c i = adjC c (\s -> s {ip = i})

putBase c b = adjC c (\s -> s {base = b})

putInputs c i = adjC c (\s -> s {inputs = i})

putOutputs c i = adjC c (\s -> s {outputs = i})

putPkt packet = modify (\s -> s {pkt = Just packet})

putLastPkt packet = modify (\s -> s {lastPkt = Just packet})

incBase c i = adjC c (\s -> s {base = i + base s})

incIP c i = adjC c (\s -> s {ip = i + ip s})

incIdleCnt :: NetworkState s ()
incIdleCnt = modify (\s -> s {idleCnt = 1 + idleCnt s})

resetIdleCnt :: NetworkState s ()
resetIdleCnt = modify (\s -> s {idleCnt = 0})

send :: [Integer] -> NetworkState s ()
send [y, x, 255] = savePacketNAT (x, y)
send [y, x, c] = do
  ins <- getsC c inputs
  putInputs c (ins |> x |> y)

savePacketNAT :: (Integer, Integer) -> NetworkState s ()
savePacketNAT (x, y) = do
  prevPkt <- gets pkt
  when (isNothing prevPkt) $ liftIO (putStrLn ("Part 1: " ++ show y))
  putPkt (x, y)

checkIdle :: NetworkState s ()
checkIdle = do
  nIdle <- gets comps <&> Map.filter (Seq.null . inputs) <&> Map.size
  when (nIdle == 50) incIdleCnt

shift offset instr = 10 ^ (offset + 1) & (instr `div`) & (`rem` 10)

getPmode c offset = do
  i <- getsC c ip
  p <- getsC c prog
  lift $ shift offset <$> readArray p i

{-# ANN readParam "Hlint: ignore Reduce duplication" #-}

readParam :: Integer -> Integer -> NetworkState s Integer
readParam c offset = do
  i <- getsC c ip
  bp <- getsC c base
  prog <- getsC c prog
  pmode <- getPmode c offset
  lift $
    case pmode of
      0 -> readArray prog >=> readArray prog $ i + offset
      1 -> readArray prog (i + offset)
      2 -> readArray prog =<< ((+ bp) <$> readArray prog (i + offset))

readParams :: Integer -> NetworkState s (Integer, Integer)
readParams c = liftM2 (,) (readParam c 1) (readParam c 2)

{-# ANN writeParam "Hlint: ignore Reduce duplication" #-}

writeParam :: Integer -> Integer -> Integer -> NetworkState s ()
writeParam c offset val = do
  i <- getsC c ip
  bp <- getsC c base
  prog <- getsC c prog
  pmode <- getPmode c offset
  m <-
    lift $
    case pmode of
      0 -> readArray prog (i + offset)
      2 -> (+ bp) <$> readArray prog (i + offset)
  void $ lift (writeArray prog m val)

main :: IO ()
main = do
  input <- filter (/= '\n') <$> readFile "data/23-puzzle-input"
  let inputs = splitString (== ',') input
      intprog = map read inputs :: [Integer]
  runSTT $ runNetwork intprog
  hFlush stdout

runNetwork :: [Integer] -> STT s IO ()
runNetwork intprog = do
  progs <- replicateM 50 $ newArray (0, 4096) 0
  forM_ progs (\p -> zipWithM_ (writeArray p) [0 ..] intprog)
  zipWith (\i p -> (i, Comp 0 0 p (Seq.singleton i) [])) [0 .. 49] progs &
    Map.fromList &
    Network 0 Nothing Nothing &
    evalStateT runNetwork'

runNetwork' :: NetworkState s ()
runNetwork' = do
  forM_ [0 .. 49] step
  checkIdle
  idleCnt <- gets idleCnt
  if idleCnt < 10000
    then runNetwork'
    else resetIdleCnt >> release

release :: NetworkState s ()
release = do
  pkt <- gets pkt
  ins <- getsC 0 inputs
  case pkt of
    Nothing     -> liftIO $ putStrLn "No packet to release"
    Just (x, y) -> putInputs 0 (ins |> x |> y) >> checkDone

checkDone :: NetworkState s ()
checkDone = do
  Just pkt@(x, y) <- gets pkt
  last <- gets lastPkt
  liftIO $ putStrLn ("NAT releasing resume: " ++ show pkt)
  if yEq y last
    then liftIO $ putStrLn ("Part 2: " ++ show y)
    else putLastPkt pkt >> runNetwork'
  where
    yEq y Nothing        = False
    yEq y (Just (_, y')) = y == y'

step :: Integer -> NetworkState s ()
step c = do
  i <- getsC c ip
  prog <- getsC c prog
  instr <- lift $ readArray prog i
  case instr `rem` 100 of
    1  -> plusMult (+) c
    2  -> plusMult (*) c
    3  -> input c
    4  -> output c
    5  -> jump (/=) c
    6  -> jump (==) c
    7  -> cond (<) c
    8  -> cond (==) c
    9  -> adjBase c
    99 -> return ()

plusMult :: IntOp -> Integer -> NetworkState s ()
plusMult op c = do
  (a, b) <- readParams c
  writeParam c 3 $ a `op` b
  incIP c 4

getInput :: Seq.Seq Integer -> NetworkState s (Seq.Seq Integer)
getInput Seq.Empty = return $ -1 :<| Seq.Empty
getInput inputs    = return inputs

input :: Integer -> NetworkState s ()
input c = do
  m :<| ins <- getInput =<< getsC c inputs
  writeParam c 1 m
  putInputs c ins
  incIP c 2

output :: Integer -> NetworkState s ()
output c = do
  m <- readParam c 1
  outs <- (m :) <$> getsC c outputs
  if length outs == 3
    then send outs >> putOutputs c []
    else putOutputs c outs
  incIP c 2

jump :: IntCond -> Integer -> NetworkState s ()
jump p c = do
  i <- getsC c ip
  (a, b) <- readParams c
  let i' =
        if p a 0
          then b
          else i + 3
  putIP c i'

cond :: IntCond -> Integer -> NetworkState s ()
cond p c = do
  (a, b) <- readParams c
  let val =
        if p a b
          then 1
          else 0
  writeParam c 3 val
  incIP c 4

adjBase :: Integer -> NetworkState s ()
adjBase c = do
  a <- readParam c 1
  incBase c a
  incIP c 2
