{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative       (optional, (<$>), (<*>))
import           Control.Monad             (liftM)
import           Data.Char                 (isSpace)
import           Data.Map                  ((!))
import qualified Data.Map.Strict           as M
import           Data.Maybe                (fromMaybe)
import qualified Data.Set                  as S
import qualified Data.Text                 as T
import           Options.Applicative       (Parser, argument, execParser,
                                            fullDesc, header, help, helper,
                                            info, long, many, metavar, option,
                                            progDesc, short, str, strOption,
                                            (<>))
import           Options.Applicative.Types (readerAsk, readerError)


type Region   = T.Text
type BugName  = T.Text
type Quantity = T.Text

type RegionQuantities = M.Map Region Quantity
type BugData          = (BugName, RegionQuantities)
type Summary          = (S.Set Region, M.Map BugName RegionQuantities)
type Frequencies      = M.Map Quantity Int
type States           = M.Map Region Int


data Task = CSV | Quantities | Risks deriving Eq

data Config = Config { task       :: Maybe Task
                     , importFrom :: Maybe String
                     , fnames     :: [String] } deriving Eq


{- Command Line Interface -}

cli :: IO Config
cli = execParser
    $ info (helper <*> opts)
      (fullDesc
    <> progDesc "Get some statistics about the bugs!"
    <> header "Bug Stats")


opts :: Parser Config
opts = Config
  <$> optional (option taskOption
      (long "task"
    <> metavar "TASK"
    <> short 't'
    <> help "what to do: (c[sv] | q[uantities] | r[isks])"))
  <*> optional (strOption
      (long "import"
    <> short 'i'
    <> metavar "CSV_FILE"
    <> help "import initital data from existing CSV-file"))
  <*> many (argument str
      (metavar "FILES..."))
  where
    taskOption = readerAsk
             >>= maybe (readerError "Wrong task name! (see --help)") return
               . (\x -> case x of
                          "c"          -> Just CSV
                          "csv"        -> Just CSV
                          "q"          -> Just Quantities
                          "quantities" -> Just Quantities
                          "r"          -> Just Risks
                          "risks"      -> Just Risks
                          _            -> Nothing)


main :: IO ()
main = do
  Config mbTask mbImportFrom fileNames <- cli
  initialSummary <- maybe (return emptySummary)
                          ((>>= return . parseSummary) . readFile)
                          mbImportFrom
  raws <- mapM readFile fileNames

  let cards = map parseCard raws
      summary = foldr collectSummary initialSummary cards
      task' = fromMaybe CSV mbTask

  case task' of
    CSV        -> printCSV summary
    Quantities -> do fs <- loadMapping "Frequencies.txt"
                     printQuantities fs summary
    Risks      -> do fs <- loadMapping "Frequencies.txt"
                     states <- loadMapping "States.txt"
                     printRisks fs states summary


{- machineria -}

loadMapping :: String -> IO (M.Map T.Text Int)
loadMapping fname = liftM (M.fromList . map mkPair . lines)
                          (readFile fname)
  where mkPair x = let (i, s) = break isSpace x
                   in  (T.strip $ T.pack s, read i :: Int)

parseSummary :: String -> Summary
parseSummary s =
  let allRows = map (split ";" . T.pack) $ lines s
      ((_:bugs):rows) = allRows
  in  foldr (collect bugs) emptySummary rows
  where
    collect :: [BugName] -> [T.Text] -> Summary -> Summary
    collect _    []       _           = error "Empty line!"
    collect bugs (reg:qs) (regs, qsm) = ( S.insert reg regs
                                        , foldr put qsm (zip bugs qs) )
      where
        put (_,   "-") m = m
        put (bug, qty) m =
          M.alter (Just . maybe (M.singleton reg qty)
                                (M.insert    reg qty)) bug m

parseCard :: String -> BugData
parseCard content =
  let (bugName:rows) = map (T.strip . T.pack) $ lines content
      quantities = foldr collect M.empty (filter (not . T.null)  rows)
  in  (bugName, quantities)
  where
    collect r m = let (qnt:regs:_) = split ":" r
                  in  foldr (`M.insert` qnt) m (split "," regs)


printCSV :: Summary -> IO ()
printCSV (regionSet, regQsByBug) =
  let regions = S.toList regionSet
      bugs = M.keys regQsByBug
      rows = map (T.unpack . T.intercalate ";")
           $ ( "Регион" : bugs )
             : map (`regionRow` bugs) regions
  in  mapM_ putStrLn rows
  where
    regionRow :: T.Text -> [T.Text] -> [T.Text]
    regionRow r = (r :) . map (get r)
    get r b = fromMaybe "-" $ M.lookup b regQsByBug >>= M.lookup r


printQuantities :: Frequencies -> Summary -> IO ()
printQuantities freqMap (regionSet, qsm) =
  let regs = S.toList regionSet
  in  mapM_ (\x -> do putStr $ T.unpack x ++ ";"
                      print (mkRow x qsm)) regs
  where
    mkRow reg = sum
              . map (maybe 0 (freqMap !) . M.lookup reg . snd)
              . M.toList


printRisks :: Frequencies -> States -> Summary -> IO ()
printRisks freqMap regMap (_, qsm) = mapM_ (putStrLn . mkRow) $ M.toList qsm
  where
    mkRow (bugName, qtyMap) =
      T.unpack bugName ++ ";" ++ show (sum prods)
      where
        prods = map prod $ M.toList qtyMap
        prod (reg, qty) = (freqMap ! qty) * (regMap ! reg)


emptySummary :: Summary
emptySummary = (S.empty, M.empty)


collectSummary :: BugData -> Summary -> Summary
collectSummary (bugName, regQs) (regionSet, regQsByBug) =
  let regionSet'  = S.union regionSet (M.keysSet regQs)
      regQsByBug' = M.alter patch bugName regQsByBug
  in  (regionSet', regQsByBug')
  where
    patch Nothing         = Just regQs
    patch (Just oldRegQs) = Just $ M.unionWith const regQs oldRegQs


split :: T.Text -> T.Text -> [T.Text]
split s = map T.strip . T.splitOn s
