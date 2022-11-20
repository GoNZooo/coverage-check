module CoverageCheck.ProjectUtilities
  ( getHpcRoot,
    findHpcReportFiles,
    streamHpcInfo,
  )
where

import Conduit
  ( ConduitT,
    MonadResource,
    await,
    concatC,
    filterC,
    mapC,
    runConduitRes,
    sequenceSources,
    yield,
    (.|),
  )
import CoverageCheck.ProjectUtilities.Types
  ( HpcFile (..),
    HpcInfo (..),
    PackageName (..),
    TestSuiteName (..),
    UnableToGetHpcDirectory (..),
  )
import Data.Conduit.Combinators (sinkList, sourceDirectoryDeep)
import Qtility
import RIO.Directory (doesDirectoryExist, listDirectory)
import RIO.FilePath (takeFileName, (</>))
import qualified RIO.Text as Text
import qualified RIO.Text.Partial as PartialText
import System.Process.Typed (byteStringOutput, proc, readProcess, setStdout, setWorkingDir)

-- | Streams individual HPC file info one by one for all available @hpc_index.html@ files in a
-- project.
streamHpcInfo ::
  forall m m'.
  (MonadUnliftIO m, MonadThrow m, MonadResource m') =>
  FilePath ->
  m (ConduitT () HpcInfo m' ())
streamHpcInfo workingDirectory = do
  hpcRoot <- getHpcRoot workingDirectory
  hpcRootFiles <- fmap (hpcRoot </>) <$> listDirectory hpcRoot
  packageDirectories <- filterM doesDirectoryExist hpcRootFiles
  let sequencedSources = sequenceSources (streamHpcFile <$> packageDirectories)
  pure (sequencedSources .| concatC)
  where
    streamHpcFile :: FilePath -> ConduitT () HpcInfo m' ()
    streamHpcFile d = do
      sourceDirectoryDeep False d
        .| filterC (Text.pack >>> ("hpc_index.html" `Text.isSuffixOf`))
        .| createHpcInfo
    createHpcInfo = do
      maybePath <- await
      case maybePath of
        Nothing -> pure ()
        Just p -> do
          case p & Text.pack & PartialText.splitOn "/" & reverse of
            _hpcFileName : testSuiteName : packageName : _ ->
              yield
                HpcInfo
                  { _hiPackageName = PackageName packageName,
                    _hiTestSuite = TestSuiteName testSuiteName,
                    _hiHpcFile = HpcFile p
                  }
            _other -> createHpcInfo

-- | Finds test suites and their associated HPC report files in a given working directory. These
-- are stored under the package names that they belong to.
findHpcReportFiles ::
  (MonadUnliftIO m, MonadThrow m) =>
  FilePath ->
  m [(PackageName, [(TestSuiteName, HpcFile)])]
findHpcReportFiles workingDirectory = do
  hpcRoot <- getHpcRoot workingDirectory
  hpcRootFiles <- fmap (hpcRoot </>) <$> listDirectory hpcRoot
  packageDirectories <- filterM doesDirectoryExist hpcRootFiles
  forM packageDirectories $ \d -> do
    hpcFiles <-
      runConduitRes $
        sourceDirectoryDeep False d
          .| filterC (Text.pack >>> ("hpc_index.html" `Text.isSuffixOf`))
          .| mapC getHpcInfo
          .| sinkList
    pure (d & takeFileName & Text.pack & PackageName, catMaybes hpcFiles)
  where
    getHpcInfo :: FilePath -> Maybe (TestSuiteName, HpcFile)
    getHpcInfo p = do
      case p & Text.pack & PartialText.splitOn "/" & reverse of
        _hpcFileName : testSuiteName : _packageName : _ ->
          Just (TestSuiteName testSuiteName, HpcFile p)
        _other -> Nothing

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
