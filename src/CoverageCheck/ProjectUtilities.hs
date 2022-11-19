module CoverageCheck.ProjectUtilities
  ( getHpcRoot,
  )
where

import Qtility
import qualified RIO.Text as Text
import System.Process.Typed (byteStringOutput, proc, readProcess, setStdout, setWorkingDir)

getHpcRoot :: FilePath -> IO FilePath
getHpcRoot workingDirectory = do
  let stackPathProcess =
        ["path", "--local-install-root"]
          & proc "stack"
          & setWorkingDir workingDirectory
          & setStdout byteStringOutput
  (exitCode, out, err) <- readProcess stackPathProcess
  case exitCode of
    ExitSuccess -> out & toStrictBytes & decodeUtf8Lenient & Text.unpack & pure
    ExitFailure _ -> error $ "Error getting stack path: " <> show err
