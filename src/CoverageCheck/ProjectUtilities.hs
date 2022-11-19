module CoverageCheck.ProjectUtilities
  ( getHpcRoot,
    findHpcReportFiles,
  )
where

import CoverageCheck.ProjectUtilities.Types (HpcFile (..), PackageName (..), UnableToGetHpcDirectory (..))
import Qtility
import RIO.Directory (doesDirectoryExist, findFile, listDirectory)
import RIO.FilePath (takeFileName, (</>))
import qualified RIO.Text as Text
import System.Process.Typed (byteStringOutput, proc, readProcess, setStdout, setWorkingDir)

-- | Finds out the HPC root directory for the given project if it is a @stack@ project. This shells
-- out to @stack@, so naturally it requires @stack@ to be installed.
getHpcRoot :: (MonadIO m, MonadThrow m) => FilePath -> m FilePath
getHpcRoot workingDirectory = do
  let stackPathProcess =
        ["path", "--local-hpc-root"]
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

-- | Finds any accessible @hpc_index.html@ files for their associated packages in a given directory.
findHpcReportFiles :: (MonadIO m, MonadThrow m) => FilePath -> m [(PackageName, Maybe HpcFile)]
findHpcReportFiles workingDirectory = do
  hpcRoot <- getHpcRoot workingDirectory
  hpcRootFiles <- fmap (hpcRoot </>) <$> listDirectory hpcRoot
  packageDirectories <- fmap takeFileName <$> filterM doesDirectoryExist hpcRootFiles
  forM packageDirectories $ \d -> do
    "hpc_index.html"
      & findFile [d]
      & fmap
        ( maybe
            (d & Text.pack & PackageName, Nothing)
            (\hpcFile -> (d & Text.pack & PackageName, hpcFile & HpcFile & Just))
        )
