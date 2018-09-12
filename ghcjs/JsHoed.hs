-- Generate the output by running
-- > ghcjs JsHoed.hs -i../src -o ../html/JsHoed
-- You can either install the ghcjs-fetch library or compile with the additional -ighcjs-fetch/src parameter.

{-# LANGUAGE PackageImports, OverloadedStrings, ScopedTypeVariables #-}

-- | Module for building an Hoed-style algorithm debugger using GHCJS.
-- The generated Javascript executable loads an Hoed @CompTree@ trace from a binary file named "CompTree" in the same folder.
module Main where

import           Debug.Hoed.CompTree
import           Debug.Hoed.Render
import           Debug.Hoed.Util
import           Safe
import           Data.Graph.Libgraph hiding (Right)
import qualified Data.Graph.Libgraph as G
import           Data.List as List
import           Data.IORef
import           Control.Monad
import           GHCJS.Types 
import           GHCJS.Marshal 
import           GHCJS.Foreign.Callback
import           GHCJS.DOM.Types 
import           GHCJS.Fetch.ByteString

-- | main function for the GHCJS executable.
main = do
    st <- initializeJsHoed
    drawJsHoed st

-- * Loading code

getCompTree :: IO CompTree
getCompTree = do
    fetchBinaryFile "CompTree"

-- * Visualization code.

drawJsHoed :: JsHoed -> IO ()
drawJsHoed st = do
    compTree <- readIORef $ jsCompTree st
    drawCompTree compTree
    currentVertex <- readIORef $ jsCurrentVertex st
    forM_ currentVertex $ selectNode . vertexId

drawCompTree :: CompTree -> IO ()
drawCompTree tree = drawCompTreeFrom tree (G.root tree) faults
    where
    faults = faultyVertices tree

drawCompTreeFrom :: CompTree -> Vertex -> [Vertex] -> IO ()
drawCompTreeFrom tree v faults = do
    drawVertex v (elem v faults)
    let nexts = succArcs tree v
    mapM_ (drawSuccArc tree faults) nexts
    
drawSuccArc :: CompTree -> [Vertex] -> Arc Vertex () -> IO ()
drawSuccArc tree faults (Arc src tgt _) = do
    drawCompTreeFrom tree tgt faults
    addEdge (vertexId src) (vertexId tgt)

drawVertex :: Vertex -> Bool -> IO ()
drawVertex RootVertex isFault = addRootNode
drawVertex (Vertex s j) isFault = do
    addNode (stmtIdentifier s) (toJSString $ prettyCompStmt s) (toJSString $ prettyFaultyJudgement j isFault)

updateJsHoedStatus :: JsHoed -> IO ()
updateJsHoedStatus st = do
    g <- readIORef $ jsCompTree st
    let isJudged v = case getJudgement v of
          G.Right -> True
          G.Wrong -> True
          _     -> False -- Assisted does not count as judged
        slen       = show . length
        ns = filter (not . isRootVertex) (preorder g)
        (js,remainingjs) = partition isJudged ns
        fs = faultyVertices g
        isFault = (length fs > 0) || null remainingjs
        txt = if (length fs > 0) then " Fault detected in: " ++ (prettyVertex . head) fs
                                 else " Judged " ++ slen js ++ "/" ++ slen ns
    updateStatus (toJSString txt) isFault
    forM_ fs $ \f -> judgeNode (vertexId f) (toJSString $ prettyFaultyJudgement G.Right True)

-- | Prettyprints a possibly faulty @Judgment@.
-- The second argument is additional information on whether a judgment is faulty.
prettyFaultyJudgement :: Judgement -> Bool -> String
prettyFaultyJudgement _ True = "faulty"
prettyFaultyJudgement j False = prettyJudgement j

-- * Visualizer's FFI.

foreign import javascript unsafe "addRootNode()"
        addRootNode :: IO ()

foreign import javascript unsafe "addNode({id: $1, label: $2, judgement: $3})"
        addNode :: Int -> JSString -> JSString -> IO ()
        
foreign import javascript unsafe "selectNode($1)"
        selectNode :: Int -> IO ()

foreign import javascript unsafe "deselectNode($1)"
        deselectNode :: Int -> IO ()

foreign import javascript unsafe "addEdge({src: $1, tgt: $2})"
        addEdge :: Int -> Int -> IO ()

foreign import javascript unsafe "updateStatus($1,$2)"
        updateStatus :: JSString -> Bool -> IO ()
        
foreign import javascript unsafe "judgeNode({id: $1, judgement: $2})"
        judgeNode :: Int -> JSString -> IO ()

foreign import javascript unsafe "document.getElementById($1).addEventListener('click',$2);"
    onClick :: JSString -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "document.getElementById($1).addEventListener('change',function(){ return $2(this.value); });"
    onRadioChange :: JSString -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "network.on('selectNode',function(params) { params.nodes.forEach($1);} );"
    onSelectNode :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "network.on('deselectNode',function(params) { params.previousSelection.nodes.forEach($1);} );"
    onDeselectNode :: Callback (JSVal -> IO ()) -> IO ()

-- * Algorithmic debugger state

data JsHoed = JsHoed
    { jsCompTree :: IORef CompTree
    , jsCurrentVertex :: IORef (Maybe Vertex)
    , jsNext :: IORef Next
    }
    
type Next = CompTree -> (Vertex -> Judgement) -> Vertex

-- | Initializes the algorithmic debugger.
-- Expects a binary file with a serialized Hoed computaation trace name "CompTree" in the same folder.
initializeJsHoed :: IO JsHoed
initializeJsHoed = do
    compTree <- getCompTree
    -- Get a list of vertices from the computation graph
    let ns = filter (not . isRootVertex) (preorder compTree)
    let isFault = length ns == 0
    updateStatus (toJSString $ " Judged 0/" ++ show (length ns)) isFault
    let currentVertex = headMay $ ns
    compTreeRef <- newIORef compTree
    currentVertexRef <- newIORef currentVertex
    nextRef <- newIORef next_step
    let st = JsHoed compTreeRef currentVertexRef nextRef
    judgeListeners st
    currentVertexListeners st
    nextListeners st
    return st

lookupVertexJsHoed :: Int -> JsHoed -> IO (Maybe Vertex)
lookupVertexJsHoed nodeId st = do
    t <- readIORef (jsCompTree st)
    return $ List.find (\v -> vertexId v == nodeId) (vertices t)

-- * Algorithmic debugger logic, mostly copied from Hoed

-- * Judge

judgeListeners :: JsHoed -> IO ()
judgeListeners st = do
    callbackRight <- syncCallback ContinueAsync $ do
        judgeJsHoed G.Right st
    onClick "right" callbackRight
    
    callbackWrong <- syncCallback ContinueAsync $ do
        judgeJsHoed G.Wrong st
    onClick "wrong" callbackWrong
        
currentVertexListeners :: JsHoed -> IO ()
currentVertexListeners st = do
    callBackSelect <- syncCallback1 ContinueAsync $ \val -> do
        Just nodeId <- fromJSVal val
        Just v <- lookupVertexJsHoed nodeId st
        writeIORef (jsCurrentVertex st) (Just v)
    onSelectNode callBackSelect
        
    callBackDeselect <- syncCallback1 ContinueAsync $ \nodeId -> do
        writeIORef (jsCurrentVertex st) Nothing
    onDeselectNode callBackDeselect

nextListeners :: JsHoed -> IO ()
nextListeners st = do
    
    callbackNext <- syncCallback ContinueAsync $ do
        advanceJsHoed st
    onClick "next" callbackNext
    
    callbackNextStrat <- syncCallback1 ContinueAsync $ \val -> do
        Just (str::String) <- fromJSVal val
        case str of
            "step" -> writeIORef (jsNext st) next_step
            "daq" -> writeIORef (jsNext st) next_daq
            otherwise -> return ()
    onRadioChange "step" callbackNextStrat
    onRadioChange "daq" callbackNextStrat

judgeJsHoed :: Judgement -> JsHoed -> IO ()
judgeJsHoed j st = do
    ct <- readIORef (jsCompTree st)
    mbv <- readIORef (jsCurrentVertex st)
    forM_ mbv $ \v -> do
        writeIORef (jsCompTree st) =<< markNode ct v j
        updateJsHoedStatus st
    advanceJsHoed st
        
advanceJsHoed :: JsHoed -> IO ()
advanceJsHoed st = do
    t <- readIORef (jsCompTree st)
    mv <- readIORef (jsCurrentVertex st)
    next <- readIORef (jsNext st)
    case (next t getJudgement) of
        RootVertex -> return ()
        w -> advanceTo w st

advanceTo :: Vertex -> JsHoed -> IO ()
advanceTo v st = do
    currentVertex <- readIORef (jsCurrentVertex st)
    forM_ currentVertex $ deselectNode . vertexId
    writeIORef (jsCurrentVertex st) (Just v)
    selectNode (vertexId v)

markNode :: CompTree -> Vertex -> Judgement -> IO CompTree
markNode g v s = mapGraphM f g
  where f RootVertex = return RootVertex
        f v'         = if v' === v then judgeNode (vertexId v) (toJSString $ prettyFaultyJudgement s False) >> return (setJudgement v s) else return v'

        (===) :: Vertex -> Vertex -> Bool
        v1 === v2 = (vertexId v1) == (vertexId v2)

-- * Utils

orError str m = m >>= \x -> case x of
    Nothing -> Prelude.error $ str
    Just x -> return x







