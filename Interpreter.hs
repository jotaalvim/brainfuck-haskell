module Interpreter where

import Data.Char
import Data.List

import Types
import ParserBrainFuck

type State = ( Int, [Int])

spred 0 = 0
spred x = pred x

add, sub, inc, dec :: State -> IO State
add (p, m) = return ( p, x ++ [succ v] ++ xs )
    where (x,v:xs) = splitAt p m

sub (p, m) = return ( p, x ++ [spred v] ++ xs )
    where (x,v:xs) = splitAt p m

inc (p, m) = return (succ p, m)
dec (p, m) = return (spred p, m)
out (p, m) = putChar c >> return (p, m)
    where c = chr $ m !! p

run :: BrainFuck -> State -> IO State
run (Add:t) state = add state >>= run t 
run (Sub:t) state = sub state >>= run t 
run (Inc:t) state = inc state >>= run t 
run (Dec:t) state = dec state >>= run t 
run (Out:t) state = out state >>= run t 

run ((Loop b):t) (s,m) = 
    if   (m !! s) /= 0
    then 
        do
            (s',m') <- run b (s,m)
            if (m' !! s') /= 0
            then run ((Loop b):t) (s',m')
            else run t (s',m')
    else run t (s,m)

run ((Cmt s):t) state = run t state

run [] state = return state

memory :: State
memory = (0 , repeat 0)

brainFuck b = (take 20 . snd) <$> run inst memory
    where inst = parserBrainFuck b

