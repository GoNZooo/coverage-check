{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module CoverageCheck.Types where

import Data.Aeson.Types (FromJSONKey, ToJSONKey)
import Qtility
import RIO.Process (HasProcessContext (..), ProcessContext)

newtype ConfigurationFileParseError = ConfigurationFileParseError
  { unConfigurationFileParseError :: String
  }
  deriving (Show)

instance Exception ConfigurationFileParseError

data HpcFileParseError = HpcFileParseError
  { _hfpeFile :: HpcFile,
    _hfpeTestSuite :: TestSuiteName,
    _hfpeContent :: Text
  }
  deriving (Show, Eq, Generic)

instance Exception HpcFileParseError

data Options = Options
  { _optionsVerbose :: Bool,
    _optionsEnvironmentFile :: EnvironmentFile,
    _optionsConfigurationFile :: Maybe FilePath,
    _optionsProjectPath :: FilePath
  }

data ConfigurationFile = ConfigurationFile
  { _cfRequirements :: Map TestSuiteName CoverageRequirement,
    _cfIgnore :: [ModuleName]
  }
  deriving (Eq, Show, Generic)

data CoverageRequirement = CoverageRequirement
  { _crTopLevelDefinitions :: Maybe Float,
    _crAlternatives :: Maybe Float,
    _crExpressions :: Maybe Float
  }
  deriving (Eq, Show, Generic)

data CoverageInfoTriple = CoverageInfoTriple
  { _citTopLevelDefinitions :: CoverageInfo,
    _citAlternatives :: CoverageInfo,
    _citExpressions :: CoverageInfo
  }
  deriving (Eq, Show, Generic)

data App = App
  { _appLogFunc :: LogFunc,
    _appProcessContext :: ProcessContext,
    _appOptions :: Options
  }

newtype ModuleName = ModuleName {unModuleName :: Text}
  deriving newtype (Eq, Ord, Show, IsString, FromJSON, ToJSON)

newtype PackageName = PackageName {unPackageName :: Text}
  deriving (Show, Eq, Ord, IsString)

newtype TestSuiteName = TestSuiteName {unTestSuiteName :: Text}
  deriving (Show, Eq, Ord, IsString, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

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
    _mhiCoverageInfo :: CoverageInfoTriple
  }
  deriving (Eq, Show, Generic)

data CoverageInfo = CoverageInfo
  { _ciPercentage :: Float,
    _ciCovered :: Int,
    _ciTotal :: Int
  }
  deriving (Eq, Show, Generic)

foldMapM
  deriveLensAndAbbreviatedJSON
  [ ''ConfigurationFile,
    ''CoverageInfoTriple,
    ''CoverageInfo,
    ''CoverageRequirement
  ]

foldMapM
  makeLenses
  [ ''Options,
    ''App,
    ''ModuleHpcInfo,
    ''HpcInfo,
    ''HpcFileParseError,
    ''TestSuiteHpcInfo
  ]

foldMapM makeWrapped [''ModuleName, ''PackageName, ''TestSuiteName, ''HpcFile]

instance HasLogFunc App where
  logFuncL = appLogFunc

instance HasProcessContext App where
  processContextL = appProcessContext
