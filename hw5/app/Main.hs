{-
Here implemented REPL.
See task function, function, which launches parser+eval
you can find in Launcher.hs
-}

module Main
  ( main
  ) where

import HW5.Launcher

import System.Console.Haskeline (InputT, defaultSettings,
                                 getInputLine, outputStrLn, runInputT)

-- | Main function for REPL.
main :: IO ()
main = runInputT defaultSettings task

-- | Main loop of hi interpreter.
task :: InputT IO ()
task = do
  prompt <- getInputLine "hi> "
  case prompt of
    Nothing     -> return ()
    Just "quit" -> return ()
    Just input  -> do
      res <- run input allPerms
      outputStrLn res >> task
