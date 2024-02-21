{-
In this file implemented function, which launches parser+eval.
I need it in tests and REPL, so it's here.
-}

module HW5.Launcher
  ( run
  , allPerms
  ) where

import Control.Monad.Cont       (lift)
import Data.Set                 (fromList)
import System.Console.Haskeline (InputT)

import HW5.Action
import HW5.Evaluator
import HW5.Parser
import HW5.Pretty
import HW5.Utility

-- | Function that launchs parsing and evaluation. See usage in tests and REPL.
run :: String -> [HiPermission] -> InputT IO String
run input perms = case parse input of
  Left err   -> show <~> err
  Right expr -> do
    ex <- lift $ runHIO (eval expr) (fromList perms)
    case ex of
      Left err    -> show <~> err
      Right value -> show <~> prettyValue value

-- | All existing permissions.
allPerms :: [HiPermission]
allPerms = [AllowRead, AllowWrite, AllowTime]
