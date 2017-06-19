module RegMachine where

import Control.Monad.State

type Prog = [Instr]

data Reg = A | B | C

data Instr = LD Reg Int
           | MOV Reg Reg
           | ADD Reg Reg Reg
           | MUL Reg Reg Reg

data Values = Values 
    { rA :: Int
    , rB :: Int
    , rC :: Int
    } deriving Show

type M = State Values

fetch :: Reg -> M Int
fetch A = do
    s <- get
    return (rA s)
fetch B = do
    s <- get
    return (rB s)
fetch C = do
    s <- get
    return (rC s)

store :: Reg -> Int -> M ()
store A v = do
    s <- get
    put s { rA = v }
store B v = do
    s <- get
    put s { rB = v }
store C v = do
    s <- get
    put s { rC = v }

exec :: Instr -> M ()
exec (LD r v) = store r v
exec (MOV r1 r2) = do
    v <- fetch r2
    store r1 v
exec (ADD r1 r2 r3) = do
    a <- fetch r2
    b <- fetch r3
    store r1 (a + b)
exec (MUL r1 r2 r3) = do
    a <- fetch r2
    b <- fetch r3
    store r1 (a * b)

execMany :: Prog -> M ()
execMany []     = return ()
execMany (x:xs) = exec x >> execMany xs

runProg :: Prog -> Values
runProg xs = snd $ runState (execMany xs) (Values 0 0 0)

main :: IO ()
main = do
    print (runProg [LD A 1, LD B 2, ADD A A B])
    print (runProg [LD A 1, LD B 2, MUL C B B, ADD A B C])
