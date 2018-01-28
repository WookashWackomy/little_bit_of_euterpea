module Main where

import MelodicFunctions
import Euterpea
import System.IO

main :: IO ()
main = do
  putStrLn "Quick example of what you can do with basic Euterpea features!"
  play $ heynow
  putStrLn "Example of invoking a recursive function that applies two functions f,g to a note,transforms it and invokes itself n times."
  play $ run2 :+: run2
  putStrLn "Type root note to hear chords based on it (major,minor and diminished) and int number (to select octave 1-8):"

  rootr <- getLine
  octaver <- getLine
  let root = read rootr :: PitchClass
  let octave = read octaver :: Int
  play $ accMaj (wn) (root,oo octave) :+: rest hn :+: accMin (wn) (root,oo octave) :+: rest hn :+: accDim (wn) (root,oo octave)
  putStrLn "Type root note, octave and a seed to hear random melody with a simple progression (lasting 8 tacts)"
  rootr <- getLine
  octaver <- getLine
  seedr <- getLine
  let seed = read seedr :: Int
  let root = read rootr :: PitchClass
  let octave = read octaver :: Int
  play $ composeWhole (root,oo octave) seed
