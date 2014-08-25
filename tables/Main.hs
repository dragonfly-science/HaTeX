{-# LANGUAGE OverloadedStrings #-}
import Prelude as P
import System.Environment (getArgs)
import System.Console.GetOpt hiding (OptArg)
import System.Directory
import Control.Monad
import Data.Either as E
import Data.List as L
import Data.Text as T hiding (foldl)
--import qualified Data.Text.IO as T
import Data.Char (isLetter)
import Text.Regex
import qualified Text.PrettyPrint.Boxes as B

import Text.LaTeX hiding (caption, label, raw)
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Parser

import Data.Csv as C
import qualified Data.ByteString.Lazy.Char8 as C


data OutputFormat = CommaSeparated | FixedWidth deriving Show
data Options = Options 
    { outputFormat :: OutputFormat 
    } deriving Show
defaultOptions :: Options
defaultOptions = Options 
    { outputFormat = FixedWidth 
    }
options :: [OptDescr (Options -> Options)]
options =
    [ Option ['f']  ["format"]
        (ReqArg (\ d opts -> opts { outputFormat = readof d }) "csv|fixed")
        "output format"
    ]
    where readof "csv" = CommaSeparated
          readof _ = FixedWidth

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
     (o,n,[]  ) -> return (L.foldl (flip id) defaultOptions o, n)
     (_,_,errs) -> ioError (userError (L.concat errs ++ usageInfo usage options))
  where usage = "Usage: textables [OPTION...] file"


main :: IO ()
main = do
    (opts, args) <- compilerOpts =<< getArgs
    let (fn, tn) = 
          case args of
            [f]    -> (f, Nothing)
            [f, t] -> (f, Just t)
            _      -> error "usage: test/runtests report.tex <table label>"
    result <- parseTexTables fn
    either (error . show) (maybe listTables (showTable opts) tn) result


listTables :: [Table] -> IO()
listTables tables = 
    let labels = [ B.text $ maybe "<no-label>" T.unpack (label t) | t <- tables]
        nrow   = [ B.text $ show (rows t)                         | t <- tables]
        ncol   = [ B.text $ show (cols t)                         | t <- tables]
        box    = B.hsep 2 B.top [ B.vcat B.left  (B.text "Label" : labels)
                                , B.vcat B.right (B.text "rows" : nrow)
                                , B.vcat B.right (B.text "cols" : ncol) 
                                ]
    in  B.printBox box


showTable :: Options -> String -> [Table] -> IO()
showTable opts tl tables = do
    let outputFunction = case outputFormat opts of
                                CommaSeparated -> commaSeparatedTable
                                _              -> fixedWidthTable
    let jtl = Just $ T.pack tl
    case L.find (\t -> label t == jtl) tables of
        Nothing  -> error $ "No table with label \"" ++ tl ++ "\" found."
        Just tab -> do 
            --mapM_ (P.putStrLn . show) $ content  tab
            --P.putStrLn $ show $ raw tab
            P.putStr $ outputFunction tab



data Table = Table {
    raw     :: LaTeX,
    caption :: Maybe Text,
    label   :: Maybe Text,
    rows    :: Int,
    cols    :: Int,
    header  :: [Row],
    content :: [Row] 
    } deriving (Show, Eq)

type Cell = (Int, Text)
type Row = [Cell]

emptytable :: LaTeX -> Table
emptytable lt = Table { 
    raw     = lt,
    caption = Nothing,
    label   = Nothing,
    rows    = 0,
    cols    = 0,
    header  = [],
    content = []
    }

parseTexTables :: FilePath -> IO (Either ParseError [Table])
parseTexTables = parseLaTeXFile >=> expandInputs >=> makeTables --(\ltx -> do warnTex ltx;  makeTables ltx)
--warnTex :: Either ParseError LaTeX -> IO()
--warnTex (Right l) = T.putStrLn ( render l)
--warnTex (Left pe) = error (show pe)

expandInputs :: Either ParseError LaTeX -> IO (Either ParseError LaTeX) 
expandInputs (Left pe) = return (Left pe)
expandInputs (Right l) = expandInput l

expandInput :: LaTeX -> IO (Either ParseError LaTeX)
expandInput (TeXComm "input" [FixArg (TeXRaw tfn)]) = do
    let fn = T.unpack tfn
    exists <- doesFileExist fn 
    let fn' = if not exists then fn ++ ".tex" else fn
    exists' <- doesFileExist fn' 
    --P.putStrLn $ "Input file: " ++ fn'
    if not exists'
        --then return (Right (TeXComment ("input file " <> tfn <> " does not exist, skipping")))
        then error $ T.unpack ("input file " <> tfn <> " does not exist, skipping")
        else parseLaTeXFile fn' >>= expandInputs
expandInput (TeXEnv n as ltx) = do
    ltx' <- expandInput ltx
    case ltx' of
        Left pe -> return (Left pe)
        Right l -> return (Right (TeXEnv n as l))
expandInput (TeXBraces ltx) = do
    ltx' <- expandInput ltx
    case ltx' of
        Left pe -> return (Left pe)
        Right l -> return (Right (TeXBraces l))
expandInput (TeXSeq ltx1 ltx2) = do
    ltx1' <- expandInput ltx1
    ltx2' <- expandInput ltx2
    case ltx1' of
        Left pe  -> return (Left pe)
        Right l1 -> do
            case ltx2' of
                Left pe -> return (Left pe)
                Right l2 -> return (Right (TeXSeq l1 l2))
expandInput (TeXComm cmd args) = do
    let expandArg :: TeXArg -> IO (Either ParseError TeXArg)
        expandArg (FixArg ltx) = do 
            ltx' <- expandInput ltx
            case ltx' of 
                Left pe -> return (Left pe)
                Right l -> return $ Right (FixArg l)
        expandArg ag  = return (Right ag)
    args' <- mapM expandArg args
    return $ Right (TeXComm cmd (E.rights args'))
expandInput ltx = return (Right ltx)

makeTables :: Either ParseError LaTeX -> IO (Either ParseError [Table])
makeTables (Left pe) = return (Left pe)
makeTables (Right l) = 
  do 
--    System.IO.putStrLn (show l)
    return (Right (makeTable l))

makeTable :: LaTeX -> [Table]
makeTable  = mktabs []
  where
    mktabs acc (TeXEnv "table" _ table) = acc ++ [ maketable (emptytable table) table ]
    mktabs acc (TeXSeq lt1 lt2) = mktabs (mktabs acc lt1) lt2
    mktabs acc (TeXBraces lt) = mktabs acc lt
    mktabs acc (TeXEnv _ _ lt) = mktabs acc lt
    mktabs acc _ = acc

    maketable tab (TeXComm "label"   [FixArg (TeXRaw lb)]) 
            = tab { label   = Just lb }
    maketable tab (TeXComm "caption" [FixArg c])           
            = tab { caption = Just (render c) }
    maketable tab (TeXEnv "tabular" [FixArg cs] rs)        
            = let c = P.filter ([(1,"")] /=) $ makerows rs
              in tab { raw     = rs
                     , content = c
                     , cols    = T.length (coldef cs)
                     , rows    = L.length c
                     }
    maketable tab (TeXEnv _ _ lt)    
            = maketable tab lt
    maketable tab (TeXBraces lt)     
            = maketable tab lt
    maketable tab (TeXComm _ args)   
            = foldl maketable tab (simpl args)
    maketable tab (TeXSeq lt1 lt2) 
            = maketable (maketable tab lt1) lt2
    maketable tab _ = tab

    coldef = T.filter (/= '@') . reducecols
    reducecols (TeXSeq (TeXRaw cls) cs) = cls <> reducecols cs
    reducecols (TeXSeq (TeXBraces _) cs) = reducecols cs
    reducecols (TeXRaw cs) = cs
    reducecols _ = ""

    simpl ((FixArg lt):as) = lt : (simpl as)
    simpl ((OptArg lt):as) = lt : (simpl as)
    simpl (_:as) = simpl as
    simpl [] = []

makerows :: LaTeX -> [Row]
makerows = L.unfoldr splitrows
  where
    splitrows :: LaTeX -> Maybe (Row, LaTeX) 
    splitrows = splitlines TeXEmpty
    splitlines acc (TeXSeq (TeXLineBreak _ _) ltx) = Just (makecols acc, ltx)
    splitlines acc (TeXSeq next ltx) = splitlines (acc <> next) ltx
    splitlines _ TeXEmpty = Nothing
    splitlines acc rest = Just (makecols (acc <> rest), TeXEmpty)

makecols :: LaTeX -> Row
makecols = L.unfoldr splitcols
  where
    splitcols :: LaTeX -> Maybe (Cell, LaTeX)
    splitcols = splitline TeXEmpty
    splitline acc (TeXSeq TeXAmp ltx) = Just (makecell acc, ltx)
    splitline acc (TeXSeq next ltx) = splitline (acc <> next) ltx
    splitline _ TeXEmpty = Nothing
    splitline acc rest = Just (makecell (acc <> rest), TeXEmpty)

makecell :: LaTeX -> Cell
makecell ltx = 
  case simplifycell ltx of
    TeXComm "multicolumn" [FixArg (TeXRaw i), _, FixArg l] ->  (read $ T.unpack i, tidycell $ render $ simplifycell l)
    l ->  (1, tidycell $ render l)

simplifycell :: LaTeX -> LaTeX
simplifycell (TeXComm "textbf" [FixArg l]) = simplifycell l
simplifycell (TeXComm "emph" [FixArg l]) = simplifycell l
simplifycell (TeXComm "mathcal" [FixArg l]) = simplifycell l
simplifycell (TeXComm "mathrm" [FixArg l]) = simplifycell l
simplifycell (TeXMath Dollar l) = simplifycell l
simplifycell (TeXCommS "addlinespace") = TeXEmpty
simplifycell (TeXCommS "mu") = TeXRaw "mu"
simplifycell (TeXCommS "sigma") = TeXRaw "sigma"
simplifycell (TeXComm "cmidrule" _) = TeXEmpty
simplifycell (TeXSeq (TeXCommS "cmidrule") (TeXSeq (TeXRaw _) (TeXSeq (TeXBraces _) a))) = simplifycell a
simplifycell (TeXComm "cellcolor" [_, _, FixArg a]) = simplifycell a
simplifycell (TeXComment _) = TeXEmpty
simplifycell (TeXRaw s) = if T.strip s == "" then TeXEmpty else (TeXRaw s)
simplifycell (TeXSeq a b) = simplifycell a <> simplifycell b
simplifycell (TeXBraces a) = simplifycell a
simplifycell a = a

tidycell :: Text -> Text 
tidycell = reduceNumbers . T.strip . (T.replace "~" " ")

reduceNumbers :: Text -> Text
reduceNumbers t = T.pack $ subRegex (mkRegex " ([0-9]{3})") (T.unpack t) "\\1"

-------------------------------------------------------------------------
-- Create an aligned version of table using boxes
-------------------------------------------------------------------------
commaSeparatedTable :: Table -> String
commaSeparatedTable tbl = 
    let expandcells = L.concat . L.map expandcell
        expandcell :: Cell -> [Cell]
        expandcell (n, t) = [(1,t)] ++ L.replicate (n-1) (1,"")
        cls = L.map expandcells $ content tbl
        csv = C.encode [ [ t | (_, t) <- c ] | c <- cls ]
    in  C.unpack $ csv

fixedWidthTable :: Table -> String
fixedWidthTable tbl = 
    let expandcells = L.concat . L.map expandcell
        expandcell :: Cell -> [Cell]
        expandcell (n, t) = [(1,t)] ++ L.replicate (n-1) (1,"")
        cls = L.transpose $ L.map expandcells $ content tbl
        alignedcols = L.map (\c -> (guessalign c, c)) cls
        bxs  = B.hsep 1 B.top [ B.vcat a [ B.text (T.unpack t) | (_, t) <- c ] | (a, c) <- alignedcols ]
    in  B.render bxs

guessalign :: [Cell] -> B.Alignment
guessalign col = 
    let hasletter :: Cell -> Int
        hasletter c | T.any isLetter $ snd $ c = 1 
                    | snd c == ""              = 1
                    | otherwise                = 0
        ratio :: Double
        ratio = (fromIntegral (sum $ L.map hasletter col)) / (fromIntegral (L.length col))
    in  if ratio > 0.5 then B.left else B.right
        
