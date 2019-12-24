import System.Environment
import Data.Char
import qualified Data.Map as Map
import Debug.Trace

-- READING MEMORY

type Mem a = Map.Map a a

split1 :: Char -> String -> ([String], String)
split1 _ [] = ([], [])

split1 delim (c:rest)
  | c == delim = ((s:ss), [])
  | space = (ss, s)
  | otherwise = (ss, (c:s))
  where
    (ss, s) = split1 delim rest
    space = isSpace c

split :: Char -> String -> [String]
split c str
  | s == [] = ss
  | otherwise = (s:ss)
  where (ss, s) = split1 c str

readMemory :: (Integral a, Read a) => String -> IO (Mem a)
readMemory fileName = do
  content <- readFile fileName
  let numbers = map read (split ',' content)
  let memory = Map.fromAscList (zip [0..] numbers)
  return memory

-- STATE

data State a = State a a (Mem a) [a] [a] deriving (Show)

state0 :: (Num a) => Mem a -> [a] -> State a
state0 mem input = (State 0 0 mem input [])

decode :: (Integral a ) => a -> a
decode instr = instr `mod` 100

argument :: (Integral a, Show a) => State a -> a -> a
argument (State ip rb mem _ _) i
  | mode == 0 = Map.findWithDefault 0 addr mem
  | mode == 1 = addr
  | mode == 2 = Map.findWithDefault 0 (addr + rb) mem
  | otherwise = error $ "Invalid mode " ++ (show mode)
  where
    mode = ((Map.findWithDefault 0 ip mem) `div` (10 ^ (i + 1))) `mod` 10
    addr = Map.findWithDefault 0 (ip + i) mem

store :: (Integral a, Show a) => State a -> a -> a -> Map.Map a a
store (State ip rb mem _ _) i value
  | mode == 0 = Map.insert addr value mem
  | mode == 2 = Map.insert (addr + rb) value mem
  | otherwise = error $ "Invalid mode " ++ (show mode)
  where
    mode = ((Map.findWithDefault 0 ip mem) `div` (10 ^ (i + 1))) `mod` 10
    addr = Map.findWithDefault 0 (ip + i) mem

add :: (Integral a, Show a) => State a -> State a
add s@(State ip rb _ input output) =
  State (ip + 4) rb mem2 input output
  where
    a1 = argument s 1
    a2 = argument s 2
    mem2 = store s 3 (a1 + a2)

mul :: (Integral a, Show a) => State a -> State a
mul s@(State ip rb _ input output) =
  State (ip + 4) rb mem2 input output
  where
    a1 = argument s 1
    a2 = argument s 2
    mem2 = store s 3 (a1 * a2)

inp :: (Integral a, Show a) => State a -> State a
inp s@(State ip rb _ (i:input) output) =
  State (ip + 2) rb mem2 input output
  where
    mem2 = store s 1 i
inp (State _ _ _ [] _) = error "No input"

outp :: (Integral a, Show a) => State a -> State a
outp s@(State ip rb mem input output) =
  State (ip + 2) rb mem input (o:output)
  where
    o = argument s 1

jmpt :: (Integral a, Show a) => State a -> State a
jmpt s@(State ip rb mem input output) =
  State ip2 rb mem input output
  where
    ip2
      | a1 /= 0 = a2
      | otherwise = ip + 3
    a1 = argument s 1
    a2 = argument s 2

jmpf :: (Integral a, Show a) => State a -> State a
jmpf s@(State ip rb mem input output) =
  State ip2 rb mem input output
  where
    ip2
      | a1 == 0 = a2
      | otherwise = ip + 3
    a1 = argument s 1
    a2 = argument s 2

lt :: (Integral a, Show a) => State a -> State a
lt s@(State ip rb _ input output) =
  State (ip + 4) rb mem2 input output
  where
    a1 = argument s 1
    a2 = argument s 2
    a
      | a1 < a2 = 1
      | otherwise = 0
    mem2 = store s 3 a

eq :: (Integral a, Show a) => State a -> State a
eq s@(State ip rb _ input output) =
  State (ip + 4) rb mem2 input output
  where
    a1 = argument s 1
    a2 = argument s 2
    a
      | a1 == a2 = 1
      | otherwise = 0
    mem2 = store s 3 a

arb :: (Integral a, Show a) => State a -> State a
arb s@(State ip rb mem input output) =
  State (ip + 2) (rb + a1) mem input output
  where
    a1 = argument s 1

halt :: (Integral a, Show a) => State a -> State a
halt (State _ rb mem input output) =
  State (-1) rb mem input output

-- PROCESSING

step :: (Integral a, Show a) => State a -> State a
step s@(State ip _ mem _ _)
  | op == 1 = add s
  | op == 2 = mul s
  | op == 3 = inp s
  | op == 4 = outp s
  | op == 5 = jmpt s
  | op == 6 = jmpf s
  | op == 7 = lt s
  | op == 8 = eq s
  | op == 9 = arb s
  | op == 99 = halt s
  | otherwise = error $ "Invalid instruction " ++ (show op)
  where
    op = decode (Map.findWithDefault 0 ip mem)

halted :: (Num a, Ord a) => State a -> Bool
halted (State ip _ _ _ _) = ip < 0

loopWhile :: (Integral a, Show a) => (State a -> Bool) -> State a -> State a
loopWhile cond s
  | not (halted s) && cond s = loopWhile cond (step s)
  | otherwise = s

-- TASK

affected :: (Integral a, Show a) => Mem a -> (a, a) -> a
affected mem (x, y) = output
  where
    st0 = state0 mem [x, y]
    State _ _ _ _ (output:_) = loopWhile (\_ -> True) st0

task1 :: (Integral a, Show a) => Mem a -> a -> a
task1 mem size = foldl (+) 0 points
  where
    lattice = [(x, y) | x <- [0..size], y <- [0..size]]
    points = map (\xy -> affected mem xy) lattice

nextPosition ::  (Integral a, Show a) => Mem a -> (a, a) -> (a, a)
nextPosition mem (x, y)
  | affected mem (x, y + 1) > 0 = (x, y + 1)
  | affected mem (x + 1, y + 1) > 0 = (x + 1, y + 1)
  | otherwise = (x + 1, y)

searchPosition :: (Integral a, Show a) => Mem a -> a -> (a, a) -> (a, a)
searchPosition mem size (x, y)
  | tr == True = (x, y)
  | otherwise = -- trace ("Position is " ++ (show x) ++ " " ++ (show y))
    (searchPosition mem size (nextPosition mem (x, y)))
  where
    tr = affected mem ((x + size - 1), (y - size + 1)) > 0

task2 :: (Integral a, Show a) => Mem a -> a -> a
task2 mem size = x * 10000 + (y - size + 1)
  where
    (x, y) = searchPosition mem size (0, size - 1)

main :: IO ()
main = do
    (a:_) <- getArgs
    mem <- readMemory a :: IO (Mem Int)
    let count = task1 mem 49
    putStrLn (show count)
    let position = task2 mem 100
    putStrLn (show position)
