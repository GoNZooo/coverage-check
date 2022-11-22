module CoverageCheck.Hpc.Html
  ( hpcInfoToTestSuiteHpcInfo,
  )
where

import CoverageCheck.Types
import Qtility
import RIO.FilePath (takeFileName)
import qualified RIO.Text as Text
import qualified RIO.Text.Partial as PartialText
import Text.HTML.Scalpel
  ( Scraper,
    anySelector,
    attr,
    chroot,
    hasClass,
    html,
    inSerial,
    matches,
    scrapeStringLike,
    seekNext,
    text,
    untilNext,
    (@:),
  )

hpcInfoToTestSuiteHpcInfo :: (MonadIO m, MonadThrow m) => HpcInfo -> m TestSuiteHpcInfo
hpcInfoToTestSuiteHpcInfo hi = do
  let testSuite = hi ^. hiTestSuite
  hpcFileContents <- readFileUtf8 $ hi ^. hiHpcFile . unwrap
  let maybeModules = scrapeStringLike hpcFileContents modulesS
  case maybeModules of
    Nothing ->
      throwM $
        HpcFileParseError
          { _hfpeFile = hi ^. hiHpcFile,
            _hfpeTestSuite = testSuite,
            _hfpeContent = hpcFileContents
          }
    Just modules -> pure $ TestSuiteHpcInfo {_tshiModules = modules, _tshiTestSuite = testSuite}

modulesS :: Scraper Text [ModuleHpcInfo]
modulesS = do
  h1Text <- optional reportGenerationErrorS
  case h1Text of
    Just () -> pure []
    Nothing -> do
      chroot ("table" @: [hasClass "dashboard"]) $
        inSerial $ do
          _tr1 <- seekNext $ matches "tr"
          _tr2 <- seekNext $ matches "tr"
          untilNext emptyTr $ many $ seekNext moduleS
  where
    emptyTr = do
      trHtml <- html "tr"
      if trHtml == "<tr></tr>"
        then pure ()
        else fail ""
    reportGenerationErrorS = do
      h1Text <- text "h1"
      if "HPC Report Generation Error" `Text.isInfixOf` h1Text
        then pure ()
        else fail "reportGenerationErrorS: not an error"

moduleS :: Scraper Text ModuleHpcInfo
moduleS = do
  moduleName <-
    ( Text.unpack
        >>> takeFileName
        >>> Text.pack
        >>> Text.stripSuffix ".hs.html"
        >>> fromMaybe ""
        >>> ModuleName
      )
      <$> attr "href" "a"
  coverageInfo <- coverageInfoS
  pure
    ModuleHpcInfo
      { _mhiName = moduleName,
        _mhiCoverageInfo = coverageInfo
      }

coverageInfoS :: Scraper Text CoverageInfoTriple
coverageInfoS = inSerial $ do
  _moduleName <- seekNext $ text "td"
  topLevelPercentage <- seekNext percentageS
  (topLevelCovered, topLevelTotal) <- seekNext coverageS
  alternativesPercentage <- seekNext percentageS
  (alternativesCovered, alternativesTotal) <- seekNext coverageS
  expressionsPercentage <- seekNext percentageS
  (expressionsCovered, expressionsTotal) <- seekNext coverageS
  pure
    CoverageInfoTriple
      { _citTopLevelDefinitions =
          CoverageInfo
            { _ciPercentage = topLevelPercentage,
              _ciCovered = topLevelCovered,
              _ciTotal = topLevelTotal
            },
        _citAlternatives =
          CoverageInfo
            { _ciPercentage = alternativesPercentage,
              _ciCovered = alternativesCovered,
              _ciTotal = alternativesTotal
            },
        _citExpressions =
          CoverageInfo
            { _ciPercentage = expressionsPercentage,
              _ciCovered = expressionsCovered,
              _ciTotal = expressionsTotal
            }
      }

percentageS :: Scraper Text Float
percentageS = do
  percentageText <- text "td"
  if "-" `Text.isInfixOf` percentageText
    then pure 0
    else do
      unless ("%" `Text.isSuffixOf` percentageText) $ fail "percentageS: no percentage"
      case percentageText & Text.stripSuffix "%" >>= (Text.unpack >>> readMaybe) of
        Nothing -> fail "percentageS"
        Just percentage -> pure percentage

coverageS :: Scraper Text (Int, Int)
coverageS = do
  coverageText <- text anySelector
  unless ("/" `Text.isInfixOf` coverageText) $ fail "coverageS: no coverage"
  let (coveredText, totalText) = PartialText.breakOn "/" coverageText
  case ( coveredText & Text.unpack & readMaybe,
         totalText & Text.stripPrefix "/" >>= (Text.unpack >>> readMaybe)
       ) of
    (Just covered, Just total) -> pure (covered, total)
    _ -> fail "coverageS: not readable as numbers"
