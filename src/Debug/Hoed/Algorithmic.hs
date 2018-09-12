{-# LANGUAGE OverloadedStrings, PackageImports, RecordWildCards #-}

-- | Module that provides an Hoed-style algorithmic debugger.
module Debug.Hoed.Algorithmic 
    ( module Debug.Hoed
    , debugAlgorithmic, copyDebugAlgorithmicFiles) where

import           Debug.Hoed hiding (debugRun)
import "Hoed"    Debug.Hoed
import           Debug.Hoed.Util
import           Debug.Hoed.Web
import           System.FilePath.Posix
import           System.FilePath.Glob
import           System.IO.Temp
import           System.Process
import           Paths_debug
import           Control.Monad
import           Data.Binary
import qualified Data.Text as T
import qualified Data.Text.IO as T
    
debugAlgorithmic :: IO a -> IO ()
debugAlgorithmic io = do
    h <- runO' defaultHoedOptions io
    debugAlgorithmicSession h
 
-- | Runs the algorithmic debugger given an Hoed trace.
debugAlgorithmicSession :: HoedAnalysis -> IO ()
debugAlgorithmicSession h = do
    withSystemTempDirectory "jshoed" $ \tmpdir -> do
        copyDebugAlgorithmicFiles tmpdir
        debugAlgorithmicOutput "." tmpdir h
        runServer tmpdir "jshoed.html"

debugAlgorithmicOutput :: FilePath -> FilePath -> HoedAnalysis -> IO ()
debugAlgorithmicOutput datapath dir h = do
    -- write trace to binary file
    encodeFile (dir </> "CompTree") (hoedCompTree h)
    -- write visualizer to  html file
    let ctx "datapath" = T.pack $ datapath
        ctx n = n
    infn <- getDataFileName "html/jshoed.html"
    tplt <- readTemplateFile infn
    let outstr = applyTemplate tplt ctx
    T.writeFile (dir </> "jshoed.html") outstr

copyDebugAlgorithmicFiles :: FilePath -> IO ()
copyDebugAlgorithmicFiles outpath = do
    infiles <- debugAlgorithmicFiles
    inpath <- getDataFileName "html"
    forM_ infiles $ \infile -> do
        let outfile = outpath </> makeRelative inpath infile
        system $ "mkdir -p " ++ (takeDirectory outfile)
        system $ "cp " ++ infile ++ " " ++ outfile

-- Images taken from Hoed.
debugAlgorithmicFiles :: IO [FilePath]
debugAlgorithmicFiles = do
    htmldir <- getDataFileName "html"
    js <- glob (htmldir </> "JSHoed.jsexe/*.js")
    png <- glob (htmldir </> "img/*.png")
    let ico = htmldir </> "favicon.ico"
    return $ js ++ png ++ [ico]
    

    