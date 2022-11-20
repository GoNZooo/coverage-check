{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module CoverageCheck.ProjectUtilities.Types where

import Qtility

data UnableToGetHpcDirectory = UnableToGetHpcDirectory
  { _utghdWorkingDirectory :: FilePath,
    _utghdStdErr :: Text,
    _utghdStdOut :: Text,
    _utghdExitCode :: ExitCode
  }
  deriving (Show, Eq)

instance Exception UnableToGetHpcDirectory

newtype PackageName = PackageName {unPackageName :: Text}
  deriving (Show, Eq, Ord, IsString)

newtype TestSuiteName = TestSuiteName {unTestSuiteName :: Text}
  deriving (Show, Eq, Ord, IsString)

newtype HpcFile = HpcFile {unHpcFile :: FilePath}
  deriving (Show, Eq, Ord, IsString)

data HpcInfo = HpcInfo
  { _hiPackageName :: PackageName,
    _hiTestSuite :: TestSuiteName,
    _hiHpcFile :: HpcFile
  }
  deriving (Eq, Show, Generic)

foldMapM makeLenses [''UnableToGetHpcDirectory, ''HpcInfo]

foldMapM makeWrapped [''PackageName, ''TestSuiteName, ''HpcFile]
