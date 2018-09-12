{-# LANGUAGE OverloadedStrings, PackageImports, RecordWildCards #-}

-- | Module that provides an Hoed-style algorithmic debugger.
module Debug.Hoed.Algorithmic 
    ( module Debug.Hoed
    , debugRun) where

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
    
debugRun :: IO a -> IO ()
debugRun io = do
    h <- runO' defaultHoedOptions io
    debugSession h
 
-- | Runs the algorithmic debugger given an Hoed trace.
debugSession :: HoedAnalysis -> IO ()
debugSession h = do
    withSystemTempDirectory "jshoed" $ \tmpdir -> do
        copyJsHoedFiles tmpdir
        encodeFile (tmpdir </> "CompTree") (hoedCompTree h)
        runServer tmpdir "jshoed.html"

copyJsHoedFiles :: FilePath -> IO ()
copyJsHoedFiles outpath = do
    infiles <- jsHoedFiles
    inpath <- getDataFileName "html"
    forM_ infiles $ \infile -> do
        let outfile = outpath </> makeRelative inpath infile
        system $ "mkdir -p " ++ (takeDirectory outfile)
        system $ "cp " ++ infile ++ " " ++ outfile

-- Images taken from Hoed.
jsHoedFiles :: IO [FilePath]
jsHoedFiles = do
    htmldir <- getDataFileName "html"
    let html = htmldir </> "jshoed.html"
    js <- glob (htmldir </> "JSHoed.jsexe/*.js")
    png <- glob (htmldir </> "img/*.png")
    let ico = htmldir </> "favicon.ico"
    return $ html : js ++ png ++ [ico]

    