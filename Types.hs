module Types where

type BrainFuck = [ Inst ]

data Inst = Add -- + Increment the byte at the data pointer by one.
          | Sub -- - Decrement the byte at the data pointer by one.
          | Dec -- < Decrement the data pointer by one (to point to the next cell to the left).
          | Inc -- > Increment the data pointer by one (to point to the next cell to the right).
          | Out -- . Output the byte at the data pointer.
          | Inp -- , Accept one byte of input, storing its value in the byte at the data pointer.
          | Loop BrainFuck
          | Cmt String --  Comment
          deriving (Eq, Show)
         
-------------------------------------------------------------------------------
-- Unparser -- Show instances
-------------------------------------------------------------------------------

-- TODO 
