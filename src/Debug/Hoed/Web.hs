{-# LANGUAGE OverloadedStrings #-}

-- | This module contains useful functions to build the visualizer web frontends.
module Debug.Hoed.Web where

import           Snap
import           Snap.Util.FileServe
import           Snap.Util.CORS
import           Web.Browser
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T
import           Data.Text.Template
import           Data.String
import           System.FilePath.Posix

-- * Minimal Snap server
-- we need to set up a web server because the Javascript fetch API forbids reading from local files.
  
runServer :: FilePath -> FilePath -> IO ()
runServer dir page = do
    openBrowser $ "http://0.0.0.0:8000" </> page
    serveSnaplet defaultConfig (initServer dir)
    return ()
  
initServer :: FilePath -> SnapletInit () ()
initServer root = makeSnaplet "site" "Site" Nothing $ do
    addRoutes [ ("/", staticHandler root)]
    return ()
    
staticHandler :: FilePath -> Handler () () ()
staticHandler root = applyCORS defaultOptions $ do
    rq <- getRequest
    let uri = B8.unpack $ rqURI rq
    serveFile (root ++ uri)

-- * Template functions

makeTemplate :: T.Text -> Template
makeTemplate str = template str

readTemplateFile :: FilePath -> IO Template
readTemplateFile fn = makeTemplate <$> T.readFile fn

applyTemplate :: Template -> Context -> T.Text
applyTemplate t c = TL.toStrict $ render t c
