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

foldMapM makeLenses [''UnableToGetHpcDirectory]
