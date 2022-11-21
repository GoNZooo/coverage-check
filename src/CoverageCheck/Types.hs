{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module CoverageCheck.Types where

import Qtility
import RIO.Process (HasProcessContext (..), ProcessContext)

data HpcFileParseError = HpcFileParseError
  { _hfpeFile :: HpcFile,
    _hfpeTestSuite :: TestSuiteName,
    _hfpeContent :: Text
  }
  deriving (Show, Eq, Generic)

instance Exception HpcFileParseError

-- | Command line arguments
data Options = Options
  { _optionsVerbose :: Bool,
    _optionsEnvironmentFile :: EnvironmentFile
  }

data App = App
  { _appLogFunc :: LogFunc,
    _appProcessContext :: ProcessContext,
    _appOptions :: Options
  }

newtype ModuleName = ModuleName {unModuleName :: Text}
  deriving newtype (Eq, Ord, Show, IsString)

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

data TestSuiteHpcInfo = TestSuiteHpcInfo
  { _tshiTestSuite :: TestSuiteName,
    _tshiModules :: [ModuleHpcInfo]
  }
  deriving (Eq, Show, Generic)

data ModuleHpcInfo = ModuleHpcInfo
  { _mhiName :: ModuleName,
    _mhiTopLevelDefinitions :: CoverageInfo,
    _mhiAlternatives :: CoverageInfo,
    _mhiExpressions :: CoverageInfo
  }
  deriving (Eq, Show, Generic)

data CoverageInfo = CoverageInfo
  { _ciPercentage :: Float,
    _ciCovered :: Int,
    _ciTotal :: Int
  }
  deriving (Eq, Show, Generic)

foldMapM
  makeLenses
  [ ''Options,
    ''App,
    ''ModuleHpcInfo,
    ''CoverageInfo,
    ''HpcInfo,
    ''HpcFileParseError
  ]

foldMapM makeWrapped [''ModuleName, ''PackageName, ''TestSuiteName, ''HpcFile]

instance HasLogFunc App where
  logFuncL = appLogFunc

instance HasProcessContext App where
  processContextL = appProcessContext
