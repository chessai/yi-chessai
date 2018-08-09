module TypeOf
  ( typeOf
  ) where

import Data.Char
import Process
import System.Environment

typeOf :: IO ()
typeOf = do
  args <- getArgs
  decl <- getContents
  let ident = fst (break isSpace decl)

  ty <- run "ghci"
    ( "-v0"
    : "-w"
    : "-outputdir /tmp/ghcinterm"
    : "-fno-warn-missing-signatures"
    : "-Wno-error=missing-signatures"
    : "-fforce-recomp"
    : "-O0"
    : "-fno-code"
    : "-fbyte-code"
    : args
    )
    (":t " ++ ident)
  putStr ty

