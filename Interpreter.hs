module Interpreter where

import Data.Char

import Types
import ParserBrainFuck

--           (data pointer index, memory list with data)
type State = ( Int, [Int])

spred 0 = 0
spred x = pred x

ssucc 127 = 0
ssucc x = succ x

add, sub, inc, dec :: State -> State

add (p, m) = ( p, x ++ [ssucc v] ++ xs )
    where (x,v:xs) = splitAt p m

sub (p, m) = ( p, x ++ [spred v] ++ xs )
    where (x,v:xs) = splitAt p m

inc (p, m) = (succ  p, m)
dec (p, m) = (spred p, m)


out :: State -> IO State
out (p, m) = putChar c >> return (p, m)

    where c = chr $ m !! p

run :: BrainFuck -> State -> IO State
run (Add:t) state = run t (add state)
run (Sub:t) state = run t (sub state)
run (Inc:t) state = run t (inc state)
run (Dec:t) state = run t (dec state)

run (Out:t) state = out state >>= run t 
run (Inp:t) state = getChar   >>= run t . (iterate add zero !!) . ord
    where zero = iterate sub state !! 127

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



