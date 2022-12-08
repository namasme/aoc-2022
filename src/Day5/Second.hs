module Day5.Second
  ( module Day5.Second
  , parseInput
  , Stack
  , Instruction(..)
  ) where

import Lens.Micro.Platform hiding (to)
import Day5.First (Instruction(..), Stack, getHeads, parseInput)

applyInstruction :: [Stack] -> Instruction -> [Stack]
applyInstruction stacks instruction = stacks
  & ix from' %~ drop (quantity instruction)
  & ix to' %~ (yanked ++)
  where
    yanked = take (quantity instruction) (stacks !! from')
    from' = from instruction - 1
    to' = to instruction - 1

applyInstructions :: [Stack] -> [Instruction] -> [Stack]
applyInstructions = foldl applyInstruction

solution :: IO ()
solution = do
  (stacks, instructions) <- parseInput <$> readFile "data/Day5/input"
  let finalStacks = applyInstructions stacks instructions

  print (getHeads finalStacks)
