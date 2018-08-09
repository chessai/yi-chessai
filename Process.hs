{-# LANGUAGE ScopedTypeVariables #-}

module Process (run) where

import System.Process
import System.Exit
import System.IO
import System.Process
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import qualified Control.Exception as C

run :: FilePath -> [String] -> String -> IO String
run file args input = C.handle (\(e :: C.IOException) -> pure (show e)) $ do

    (inp,out,err,pid) <- runInteractiveProcess file args Nothing Nothing
    hPutStr inp input >> hClose inp

    output <- hGetContents out
    -- errput <- hGetContents err

    outMVar <- newEmptyMVar
    -- errMVar <- newEmptyMVar

    forkIO (C.evaluate (length output) >> putMVar outMVar ())
    -- forkIO (C.evaluate (length errput) >> putMVar errMVar ())

    takeMVar outMVar
    -- takeMVar errMVar

    _ <- C.catch
            (waitForProcess pid)
            (\(_ :: C.IOException) -> pure ExitSuccess)

    pure (output)
