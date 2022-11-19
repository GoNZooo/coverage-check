module CoverageCheck.ProjectUtilities
  ( getHpcRoot,
  )
where

import CoverageCheck.ProjectUtilities.Types (UnableToGetHpcDirectory (..))
import Qtility
import qualified RIO.Text as Text
import System.Process.Typed (byteStringOutput, proc, readProcess, setStdout, setWorkingDir)

-- | Finds out the HPC root directory for the given project if it is a @stack@ project. This shells
-- out to @stack@, so naturally it requires @stack@ to be installed.
getHpcRoot :: (MonadIO m, MonadThrow m) => FilePath -> m FilePath
getHpcRoot workingDirectory = do
  let stackPathProcess =
        ["path", "--local-install-root"]
          & proc "stack"
          & setWorkingDir workingDirectory
          & setStdout byteStringOutput
  (exitCode, out, err) <- readProcess stackPathProcess
  case exitCode of
    ExitSuccess -> out & toStrictBytes & decodeUtf8Lenient & Text.strip & Text.unpack & pure
    ExitFailure _ ->
      throwM $
        UnableToGetHpcDirectory
          { _utghdWorkingDirectory = workingDirectory,
            _utghdStdErr = err & toStrictBytes & decodeUtf8Lenient,
            _utghdStdOut = out & toStrictBytes & decodeUtf8Lenient,
            _utghdExitCode = exitCode
          }
