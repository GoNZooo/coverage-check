module Run (run) where

import Conduit (runConduitRes, sinkList, (.|))
import CoverageCheck.Hpc.Html (hpcInfoToTestSuiteHpcInfo)
import CoverageCheck.ProjectUtilities (streamHpcInfo)
import CoverageCheck.Types
import Data.Yaml (decodeFileThrow)
import Qtility
import qualified RIO.Map as Map
import RIO.Process (mkDefaultProcessContext)

run :: Options -> IO ()
run options = do
  loadDotEnvFile $ options ^. optionsEnvironmentFile
  lo <- logOptionsHandle stderr (options ^. optionsVerbose)
  pc <- mkDefaultProcessContext
  configurationFile <- forM (options ^. optionsConfigurationFile) $ \p -> do
    decodeFileThrow p
  withLogFunc lo $ \lf ->
    let app =
          App
            { _appLogFunc = lf,
              _appProcessContext = pc,
              _appOptions = options
            }
     in runRIO app $ runApp options configurationFile

runApp :: Options -> Maybe ConfigurationFile -> RIO App ()
runApp options maybeConfiguration = do
  hpcInfos <- streamHpcInfo (options ^. optionsProjectPath) >>= \s -> runConduitRes $ s .| sinkList
  testSuiteHpcInfos <- mapM hpcInfoToTestSuiteHpcInfo hpcInfos
  forM_ testSuiteHpcInfos $ \tshi -> do
    forM_ maybeConfiguration $ \c -> checkCoverage tshi c
    logCoverage tshi
  where
    logCoverage :: TestSuiteHpcInfo -> RIO App ()
    logCoverage tshi = do
      let totals = calculateTotals tshi
      logInfo $ display $ tshi ^. tshiTestSuite . unwrap
      forM_ (tshi ^. tshiModules) $ \m -> do
        if moduleIsIgnored m maybeConfiguration
          then pure ()
          else logInfo $ display $ "  " <> displayModuleHpcInfo m
      logInfo $
        display $
          mconcat
            [ "  Totals:\n    Top-level definitions: ",
              displayCoverage (totals ^. citTopLevelDefinitions),
              "\n    Alternatives: ",
              displayCoverage (totals ^. citAlternatives),
              "\n    Expressions: ",
              displayCoverage (totals ^. citExpressions)
            ]

    displayModuleHpcInfo mhi =
      mconcat
        [ display (mhi ^. mhiName . unwrap),
          "\n    Top-level definitions: ",
          displayCoverage (mhi ^. mhiCoverageInfo . citTopLevelDefinitions),
          "\n    Alternatives: ",
          displayCoverage (mhi ^. mhiCoverageInfo . citAlternatives),
          "\n    Expressions: ",
          displayCoverage (mhi ^. mhiCoverageInfo . citExpressions)
        ]

    displayCoverage ci =
      display (ci ^. ciPercentage)
        <> "% ("
        <> display (ci ^. ciCovered)
        <> "/"
        <> display (ci ^. ciTotal)
        <> ")"

    calculateTotals :: TestSuiteHpcInfo -> CoverageInfoTriple
    calculateTotals tshi =
      foldr addModule emptyCoverageInfoTriple (tshi ^. tshiModules)

    emptyCoverageInfoTriple =
      CoverageInfoTriple (CoverageInfo 0 0 0) (CoverageInfo 0 0 0) (CoverageInfo 0 0 0)

    addModule :: ModuleHpcInfo -> CoverageInfoTriple -> CoverageInfoTriple
    addModule mhi cit =
      if moduleIsIgnored mhi maybeConfiguration
        then cit
        else
          CoverageInfoTriple
            ( addCoverageInfo
                (mhi ^. mhiCoverageInfo . citTopLevelDefinitions)
                (cit ^. citTopLevelDefinitions)
            )
            ( addCoverageInfo
                (mhi ^. mhiCoverageInfo . citAlternatives)
                (cit ^. citAlternatives)
            )
            ( addCoverageInfo
                (mhi ^. mhiCoverageInfo . citExpressions)
                (cit ^. citExpressions)
            )

    addCoverageInfo :: CoverageInfo -> CoverageInfo -> CoverageInfo
    addCoverageInfo ci1 ci2 =
      let newCovered = ci1 ^. ciCovered + ci2 ^. ciCovered
          newTotal = ci1 ^. ciTotal + ci2 ^. ciTotal
       in CoverageInfo
            { _ciPercentage = (fromIntegral newCovered / fromIntegral newTotal) * 100,
              _ciCovered = newCovered,
              _ciTotal = newTotal
            }

    checkCoverage :: TestSuiteHpcInfo -> ConfigurationFile -> RIO App ()
    checkCoverage tshi c = do
      let totals = calculateTotals tshi
          required =
            c ^. cfRequirements
              & Map.lookup (tshi ^. tshiTestSuite)
              & fromMaybe
                CoverageRequirement
                  { _crTopLevelDefinitions = Nothing,
                    _crAlternatives = Nothing,
                    _crExpressions = Nothing
                  }
          totalTopLevelPercentage = totals ^. citTopLevelDefinitions . ciPercentage
          totalTopLevelCovered = totals ^. citTopLevelDefinitions . ciCovered
          totalTopLevelTotal = totals ^. citTopLevelDefinitions . ciTotal
          requiredTopLevel = required ^. crTopLevelDefinitions & fromMaybe 0
          totalAlternativesPercentage = totals ^. citAlternatives . ciPercentage
          totalAlternativesCovered = totals ^. citAlternatives . ciCovered
          totalAlternativesTotal = totals ^. citAlternatives . ciTotal
          requiredAlternatives = required ^. crAlternatives & fromMaybe 0
          totalExpressionsPercentage = totals ^. citExpressions . ciPercentage
          totalExpressionsCovered = totals ^. citExpressions . ciCovered
          totalExpressionsTotal = totals ^. citExpressions . ciTotal
          requiredExpressions = required ^. crExpressions & fromMaybe 0
          anyError =
            totalTopLevelPercentage < requiredTopLevel
              || totalAlternativesPercentage < requiredAlternatives
              || totalExpressionsPercentage < requiredExpressions
      when anyError $ do
        logError $ display (tshi ^. tshiTestSuite . unwrap)
      when (totalTopLevelPercentage < requiredTopLevel) $ do
        logError $
          display $
            mconcat
              [ "  Top-level definitions coverage is below the required threshold of ",
                display (required ^. crTopLevelDefinitions & fromMaybe 0),
                "% (",
                display totalTopLevelCovered,
                "/",
                display totalTopLevelTotal,
                " = ",
                display totalTopLevelPercentage,
                "%)"
              ]
      when (totalAlternativesPercentage < requiredAlternatives) $ do
        logError $
          display $
            mconcat
              [ "  Alternatives coverage is below the required threshold of ",
                display (required ^. crAlternatives & fromMaybe 0),
                "%; (",
                display totalAlternativesCovered,
                "/",
                display totalAlternativesTotal,
                " = ",
                display totalAlternativesPercentage,
                "%)"
              ]
      when (totalExpressionsPercentage < requiredExpressions) $ do
        logError $
          display $
            mconcat
              [ "  Expressions coverage is below the required threshold of ",
                display (required ^. crExpressions & fromMaybe 0),
                "% (",
                display totalExpressionsCovered,
                "/",
                display totalExpressionsTotal,
                " = ",
                display totalExpressionsPercentage,
                "%)"
              ]
      when anyError exitFailure

    moduleIsIgnored :: ModuleHpcInfo -> Maybe ConfigurationFile -> Bool
    moduleIsIgnored mhi maybeConfig =
      maybe False (isIgnored mhi) maybeConfig

    isIgnored :: ModuleHpcInfo -> ConfigurationFile -> Bool
    isIgnored mhi c =
      mhi ^. mhiName `elem` c ^. cfIgnore
