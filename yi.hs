import Yi                               (Action (..), BufferM, Region, YiM)
import Yi.Config.Default.HaskellMode    (configureHaskellMode)
import Yi.Config.Default.JavaScriptMode (configureJavaScriptMode)
import Yi.Config.Default.MiscModes      (configureMiscModes)
import Yi.Config.Default.Vim            (configureVim)
import Yi.Config.Default.Vty            (configureVty)
import Yi.Config.Simple.Types           (ConfigM, runConfigM)
import Yi.Mode.Common                   (TokenBasedMode)
import Yi.String                        (fillText)

import Yi.Config.Simple
    (TextUnit (..), addMode, atEof, atSol, ctrlCh, defaultConfig,
    deleteRegionB, globalBindKeys, inclusiveRegionB, lineDown, metaCh,
    modePrettify, modePrettifyA, modifyRegionB, newTabE, openNewFile,
    regionOfB, startActionsA, startEditor, unitParagraph, withCurrentBuffer,
    withSyntax, (?>>!))

import qualified Yi.Lexer.GitCommit as GitCommit
import qualified Yi.Modes
import qualified Yi.Rope            as R

import Control.Lens             ((.~))
import Control.Monad            (unless)
import Control.Monad.State.Lazy (execStateT)
import Data.Function            ((&))
import Data.List                (intersperse)
import Lens.Micro.Platform      ((.=))
import System.Environment       (getArgs)

main :: IO ()
main = do
  args <- getArgs

  let
    files = case args of
      -- If no args are given, open the current directory 
      [] -> ["."]
      xs -> xs
    
    openFileActions = intersperse (EditorA newTabE)
                        (map (YiA . openNewFile) files)

    config = do
      configureVim
      configureVty
      configureHaskellMode
      addMode Yi.Modes.cabalMode
      addMode Yi.Modes.cppMode
      addMode Yi.Modes.gitCommitMode
      addMode Yi.Modes.jsonMode
      addMode Yi.Modes.whitespaceMode
      
      startActionsA .= openFileActions
  
  cfg <- execStateT (runConfigM config) defaultConfig

  startEditor cfg Nothing
