module Main where

import Distribution.Simple
import System.Process
import Control.Monad
import System.FilePath
import System.Directory

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
      { buildHook = \pkg lbi hooks flags -> do
          compileCoffeeScripts
          buildHook simpleUserHooks pkg lbi hooks flags
      }

compileCoffeeScripts :: IO ()
compileCoffeeScripts = do
  let scriptsDir = "." </> "Web" </> "scripts"
  files <- getDirectoryContents scriptsDir
  forM_ files $ \file -> when (takeExtension file == ".coffee") $
    void $ readProcess "coffee" ["-b", "-c", scriptsDir </> file] ""
