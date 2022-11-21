module Run (run) where

import CoverageCheck.Types
import Qtility
import RIO.Process (mkDefaultProcessContext)

run :: Options -> IO ()
run options = do
  loadDotEnvFile $ options ^. optionsEnvironmentFile
  lo <- logOptionsHandle stderr (options ^. optionsVerbose)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app =
          App
            { _appLogFunc = lf,
              _appProcessContext = pc,
              _appOptions = options
            }
     in runRIO app runApp

runApp :: RIO App ()
runApp = do
  logInfo "We're inside the application!"
