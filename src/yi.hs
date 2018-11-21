{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Yi                               (Action (..), BufferM, Region, YiM)
import Yi.Config.Default.HaskellMode    (configureHaskellMode)
import Yi.Config.Default.JavaScriptMode (configureJavaScriptMode)
import Yi.Config.Default.MiscModes      (configureMiscModes)
import Yi.Config.Default.Vim            (configureVim)
import Yi.Config.Default.Vty            (configureVty)
import Yi.Config.Simple.Types           (ConfigM, runConfigM)
import Yi.Mode.Common                   (TokenBasedMode)
import Yi.String                        (fillText)
import Yi.Types                         (Config)

import Yi.Config.Simple
    (TextUnit (..), addMode, atEof, atSol, ctrlCh, defaultConfig,
    deleteRegionB, globalBindKeys, inclusiveRegionB, lineDown, metaCh,
    modePrettify, modePrettifyA, modifyRegionB, newTabE, openNewFile,
    regionOfB, startActionsA, startEditor, unitParagraph, withCurrentBuffer,
    withSyntax, (?>>!), (?>>))

import qualified Yi.Lexer.GitCommit as GitCommit
import qualified Yi.Modes
import qualified Yi.Rope            as R

import Control.Lens             ((.~))
import Control.Monad            (unless)
import Control.Monad.State.Lazy (execStateT)
import Data.Char                (isSpace)
import Data.Function            ((&))
import Data.List                (intersperse)
import Lens.Micro.Platform      ((.=))
import System.Environment       (getArgs)
import Process                  (run)

getStuff :: IO [String]
getStuff = getArgs >>= \case
  [] -> pure ["."]
  xs -> pure xs

openFileActions :: [String] -> [Action]
openFileActions files =
  intersperse (EditorA newTabE)
    (map (YiA . openNewFile) files)

config :: ConfigM ()
config = do
  configureVim
  configureVty
  configureHaskellMode
  addMode Yi.Modes.cMode 
  addMode Yi.Modes.cabalMode
  addMode Yi.Modes.cppMode
  addMode Yi.Modes.gitCommitMode
  addMode Yi.Modes.jsonMode
  addMode Yi.Modes.whitespaceMode

  globalBindKeys $ ctrlCh 'd' ?>> ctrlCh 'd' ?>>! delLine

main :: IO ()
main = do
  files <- getStuff
  let _config = config >> startActionsA .= (openFileActions files) 
  cfg <- execStateT (runConfigM _config) defaultConfig
  startEditor cfg Nothing

delLine :: YiM ()
delLine = withCurrentBuffer delLineB

delLineB :: BufferM ()
delLineB = do
  e <- (&&) <$> atEof <*> atSol
  unless e $ do
    r <- inclusiveRegionB =<< regionOfB Line
    lineDown
    deleteRegionB r

typeOf :: String -> IO String
typeOf decl = do
  let ident = fst (break isSpace decl)

  run "ghci"
    [ "-v0"
    , "-w"
    , "-outputdir /tmp/ghcinterm"
    , "-fno-warn-missing-signatures"
    , "-Wno-error=missing-signatures"
    , "-fforce-recomp"
    , "-O0"
    , "-fno-code"
    , "-fbyte-code"
    ]
    (":t " ++ ident)

--infer :: BufferM ()
--infer = do
--  r :: Region <- inclusiveRegionB =<< regionOfB Line
--  lineUp
--  ty <- 