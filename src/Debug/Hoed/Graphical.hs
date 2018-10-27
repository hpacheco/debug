{-# LANGUAGE ViewPatterns, OverloadedStrings, PackageImports, DeriveGeneric, RecordWildCards #-}

-- | Module that provides an Hoed-style algorithmic debugger.
module Debug.Hoed.Graphical 
    ( module Debug.Hoed
    , debugGraphical, debugGraphicalOutputToFile,mkJsTrace) where

import           Debug.Hoed hiding (debugRun)
import "Hoed"    Debug.Hoed
import           Debug.Hoed.Util
import           Debug.Hoed.Observe
import           Debug.Hoed.CompTree
import           Debug.Hoed.Web
import           System.IO.Temp
import           Paths_debug
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Aeson as Aeson
import           Data.Map (Map(..))
import qualified Data.Map as Map
import           Data.HashMap.Strict (HashMap(..))
import qualified Data.HashMap.Strict as HashMap
import           Data.IntMap (IntMap(..))
import qualified Data.IntMap as IMap
import           Data.IntSet (IntSet(..))
import qualified Data.IntSet as ISet
import           Control.Monad.State (State(..))
import qualified Control.Monad.State as State
import           Data.Maybe
import qualified Data.Vector.Generic as GV
import           GHC.Word
import           Web.Browser
import           System.IO
import           Control.Concurrent
import           Control.Monad
import           Safe

import Debug.Trace

-- * Graphical debugger invocation
    
debugGraphical :: IO a -> IO ()
debugGraphical io = do
    h <- runO' defaultHoedOptions io
    debugGraphicalSession h
 
debugGraphicalOutput :: HoedAnalysis -> IO T.Text
debugGraphicalOutput h = do
    let tr = hoedTrace h
    ti <- traceInfo Silent tr
    let jstr = mkJsTrace tr ti
--    forM jstr $ \e -> putStrLn $ show e
    let jsons = show $ map encode jstr
    let ctx "jsEvents" = T.pack jsons
        ctx n = n
    infn <- getDataFileName "html/jshood.html"
    tplt <- readTemplateFile infn
    let outstr = applyTemplate tplt ctx
    return outstr
 
debugGraphicalOutputToFile :: FilePath -> HoedAnalysis -> IO () 
debugGraphicalOutputToFile file h = do
    debugGraphicalOutput h >>= T.writeFile file

-- | Runs the graphical debugger given an Hoed trace.
debugGraphicalSession :: HoedAnalysis -> IO ()
debugGraphicalSession h = do
    outstr <- debugGraphicalOutput h
    withSystemTempFile "jshood.html" $ \tmpfile hdl -> do
        T.hPutStr hdl outstr
        hFlush hdl
        hClose hdl
        openBrowser tmpfile
        forever $ threadDelay (10^6) -- to prevent thee file to be deleted too early
        return ()

-- * Graphical debugger @JsEvent@s.
-- An event represents an animation step.

--data JsId = JsFun { jsNodeId :: Int }
--          | JsCon { jsNodeId :: Int, jsConId :: Int  }
--  deriving (Generic,Show)
--
--prettyJsId :: JsId -> String
--prettyJsId (JsFun uid) = "n"++show uid
--prettyJsId (JsCon uid cid) = "c"++show cid
--
--instance ToJSON JsId where
--    toJSON = String . T.pack . prettyJsId

type JsId = Int

jsNodeId = id

type JsLabel = String

data JsParent = JsParent { jsParentId :: JsId, jsParentType :: JsEdgeType }
  deriving (Generic,Show)

instance ToJSON JsParent where
    toEncoding = genericToEncoding defaultOptions

data JsEdgeType = JsInput | JsOutput | JsCall | JsDataType
  deriving (Generic,Show)

instance ToJSON JsEdgeType where
    toEncoding = genericToEncoding defaultOptions

data JsChild = JsChild { jsChildId :: JsId, jsChildType :: JsEdgeType }
  deriving (Generic,Show)

instance ToJSON JsChild where
    toEncoding = genericToEncoding defaultOptions

-- | An Hoed event for visualization, extended with parent/child node information.
data JsEvent = JsEvent { jsId :: JsId, jsType :: JsType, jsChildren :: [JsChild], jsParent :: (Maybe JsParent), jsLabel :: (Maybe JsLabel) }
  deriving (Generic,Show)

data JsType
    = JsNew -- ^ Construct a new thunk
    | JsUpd -- ^ Update an existing thunk with its computed value
  deriving (Generic,Show)
    
instance ToJSON JsType where
    toEncoding = genericToEncoding defaultOptions

jsNew :: JsId -> [JsChild] -> (Maybe JsParent) -> (Maybe JsLabel) -> JsEvent
jsNew jid jchi jpar jlbl = JsEvent jid JsNew jchi jpar jlbl

jsUpd :: JsId -> [JsChild] -> Maybe JsParent -> JsLabel -> JsEvent
jsUpd jid jchi jpar jlbl = JsEvent jid JsUpd jchi jpar (Just jlbl)
    
instance ToJSON JsEvent where
    toEncoding = genericToEncoding defaultOptions

-- A @JsTrace@ is a sequence of @JsEvent@s.
-- Since every @JsEvent@ is annotated with parent/child information, we can animate 
type JsTrace = [JsEvent]

-- * Convert Hoed trace information to a @JsTrace@

data JsState = JsState
    { jsIds :: JsIds -- map from nodes to ids
    , jsLabels :: JsLabels -- map from nodes to labels
    , jsChilds :: JsChildren  -- ordered list of children of a node
    , jsAliases :: JsAliases -- parent aliases, to convert binary functions to n-ary functions
    , jsFuns :: JsFuns
    , jsShared :: JsShared
    , jsObserveFuns :: JsFuns
    , jsDeadFuns :: JsFuns
    , jsTraceInfo :: JsTraceInfo -- map from childs to parents
    , jsThunk :: Int -- counter for unique node ids that are not evaluated
    }
  deriving (Generic,Show)
  
type JsIds = IntMap JsId
type JsLabels = IntMap JsLabel 
type JsChildren = IntMap [Int]
type JsAliases = Map Parent Parent
type JsFuns = IntSet
type JsShared = IntSet

type JsTraceInfo = IntMap Int 

-- | Convert Hoed trace information to a @JsTrace@
mkJsTrace :: Trace -> TraceInfo -> JsTrace
mkJsTrace tr ti = reverse $ State.evalState (mkJsTraceSt tr) ini
    where ini = initializeJsState tr ti

initializeJsState :: Trace -> TraceInfo -> JsState
initializeJsState tr ti = st
    where
    emptyJsState = JsState IMap.empty IMap.empty IMap.empty Map.empty ISet.empty ISet.empty ISet.empty ISet.empty (mkJsTraceInfo ti) (-1)
    st = GV.ifoldl initializeJsEvent emptyJsState (GV.unsafeTail tr)
    initializeJsEvent :: JsState -> Int -> Event -> JsState
    initializeJsEvent st (succ -> uid) (Event oldparent@(flip canonicalParent st -> parent) change) = {-trace (show uid ++ ":" ++ show change) $ -}case change of
        Observe s -> flip State.execState st $ do
            let jsid = uid --JsFun uid
            State.modify $ addJsId uid jsid
            State.modify $ addJsLabel jsid (T.unpack (trimText s))
        Enter -> st
        Fun -> flip State.execState st $ do
            let jsid = uid --JsFun uid
            State.modify $ addJsId uid jsid
            pjsid <- State.gets $ getJsId $ parentUID parent
            isParentFun <- State.gets $ isJsFun pjsid
            {-trace (show uid ++ ": isParentFun " ++ show isParentFun) $-}
            if isParentFun && parentPosition oldparent == 1
                then do
                    State.modify $ addJsChildrenAliases jsid parent
                    --State.modify $ addJsFun jsid
                    State.modify $ addJsDeadFun jsid
                else State.modify $ addJsFun jsid
            pjsid <- State.gets $ getJsId $ parentUID parent
            State.modify $ copyJsLabel pjsid jsid
            State.modify $ addJsChild jsid parent
        Cons cid n s -> flip State.execState st $ do
            let jsid = maybe uid id cid
            State.modify $ addJsId uid jsid
            State.modify $ addJsChild jsid parent
            State.modify $ addJsShared uid parent cid
        ConsChar cid c -> flip State.execState st $ do
            let jsid = maybe uid id cid
            State.modify $ addJsId uid jsid
            State.modify $ addJsChild jsid parent
            State.modify $ addJsShared uid parent cid

addJsShared :: UID -> Parent -> Sharing -> JsState -> JsState
addJsShared uid _ (Just _) st = st { jsShared = ISet.insert uid (jsShared st) }
addJsShared uid (Parent puid _) Nothing st = if isJsShared puid st
    then st { jsShared = ISet.insert uid (jsShared st) }
    else st

isJsShared :: UID -> JsState -> Bool
isJsShared uid st = ISet.member uid (jsShared st)

addJsId :: UID -> JsId -> JsState -> JsState
addJsId uid jsid st = st { jsIds = IMap.insert uid {-(jsNodeId jsid)-} jsid (jsIds st) }

getJsId :: UID -> JsState -> JsId
getJsId uid st | uid < 0 = uid
getJsId uid st = case IMap.lookup uid (jsIds st) of
    Nothing -> error $ "getJsId " ++ show uid ++ " in \n" ++ show (jsIds st)
    Just jsid -> jsid

copyJsLabel :: JsId -> JsId -> JsState -> JsState
copyJsLabel from to st = case IMap.lookup (jsNodeId from) (jsLabels st) of
    Nothing -> st
    Just lbl -> st { jsLabels = IMap.insert (jsNodeId to) (lbl) $ jsLabels st, jsObserveFuns = ISet.insert (jsNodeId from) $ jsObserveFuns st }

isJsDeadFun :: JsId -> JsState -> Bool
isJsDeadFun jsid st = ISet.member (jsNodeId jsid) (jsDeadFuns st)

isJsObserveFun :: JsId -> JsState -> Bool
isJsObserveFun jsid st = ISet.member (jsNodeId jsid) (jsObserveFuns st)

addJsDeadFun :: JsId -> JsState -> JsState
addJsDeadFun jsid st = st { jsDeadFuns = ISet.insert (jsNodeId jsid) $ jsDeadFuns st }

addJsLabel :: JsId -> JsLabel -> JsState -> JsState
addJsLabel jsid lbl st = st { jsLabels = IMap.insert (jsNodeId jsid) lbl $ jsLabels st }

isJsFun :: JsId -> JsState -> Bool
isJsFun jsid st = ISet.member (jsNodeId jsid) (jsFuns st)

addJsFun :: JsId -> JsState -> JsState
addJsFun jsid st = st { jsFuns = ISet.insert (jsNodeId jsid) $ jsFuns st }

addJsChildrenAliases :: JsId -> Parent -> JsState -> JsState
addJsChildrenAliases jsid parent st = case canonicalParent parent st of
    p@(Parent puid pidx) -> st { jsAliases = Map.insert (Parent (jsNodeId jsid) 1) (Parent puid $ pidx+1) $ Map.insert (Parent (jsNodeId jsid) 0) p $ jsAliases st }

addJsChild :: JsId -> Parent -> JsState -> JsState
addJsChild jsid p st = st { jsThunk = thunk-fromEnum cpidx, jsChilds = IMap.alter (Just . ins cpidx (jsNodeId jsid) . maybe [] id) cpuid $ jsChilds st }
    where
    thunk = jsThunk st
    (Parent cpuid cpidx) = canonicalParent p st
    ins :: Word8 -> Int -> [Int] -> [Int]
    ins 0 v [] = [v]
    ins 0 v (x:xs) = v:xs
    ins n v [] = (thunk-fromEnum n) : ins (n-1) v []
    ins n v (x:xs) = x : ins (n-1) v xs

canonicalParent :: Parent -> JsState -> Parent
canonicalParent p st = case Map.lookup p (jsAliases st) of
    Nothing -> p
    Just p' -> canonicalParent p' st

mkJsTraceInfo :: TraceInfo -> JsTraceInfo
mkJsTraceInfo = IMap.foldrWithKey go IMap.empty . dependencies
    where
    go :: Int -> IntSet -> JsTraceInfo -> JsTraceInfo
    go from tos m | from > 0 = ISet.foldr (\to -> IMap.insert to from) m (ISet.filter (>0) tos)
                  | otherwise = m
    
mkJsTraceSt :: Trace -> State JsState JsTrace
mkJsTraceSt = GV.ifoldl addJsEventSt (return []) . GV.unsafeTail
    where
    addJsEventSt :: State JsState JsTrace -> Int -> Event -> State JsState JsTrace
    addJsEventSt mtr (succ -> i) e = do
        tr <- mtr
        te <- mkJsEventSt i e
        return (maybeToList te ++ tr)

mkJsEventSt :: Int -> Event -> State JsState (Maybe JsEvent)
mkJsEventSt uid (Event parent change) = do
    case change of
        Observe s -> do
            jsid <- State.gets (getJsId uid)
            isFun <- State.gets $ isJsObserveFun jsid
            if isFun
                then return Nothing
                else do
                    ch <- State.gets $ mkJsChildren False jsid
                    par <- State.gets $ mkJsParent False jsid parent
                    lbl <- State.gets $ mkJsLabel jsid
                    return $ Just $ jsNew jsid ch par lbl
        Enter -> return Nothing
        Fun -> do
            jsid <- State.gets (getJsId uid)
            isDeadFun <- State.gets $ isJsDeadFun jsid
            if isDeadFun
                then return Nothing
                else do
                    ch <- State.gets $ mkJsChildren False jsid
                    par <- State.gets $ mkJsParent False jsid parent
                    lbl <- State.gets $ mkJsLabel jsid
                    return $ Just $ jsNew jsid ch par lbl
        Cons _ _ s -> do
            isShared <- State.gets $ isJsShared uid
            if isShared then return Nothing else do
                jsid <- State.gets $ getJsId uid
                ch <- State.gets $ mkJsChildren True jsid
                par <- State.gets $ mkJsParent True jsid parent
                return $ Just $ jsUpd jsid ch par (T.unpack s)
        ConsChar _ c -> do
            isShared <- State.gets $ isJsShared uid
            if isShared then return Nothing else do 
                jsid <- State.gets $ getJsId uid
                ch <- State.gets $ mkJsChildren True jsid
                par <- State.gets $ mkJsParent True jsid parent
                return $ Just $ jsUpd jsid ch par [c]

mkJsChildren :: Bool -> JsId -> JsState -> [JsChild]
mkJsChildren isCons jsid st = maybe [] add $ IMap.lookup (jsNodeId jsid) (jsChilds st)
    where
    add xs = map (uncurry JsChild) $ zip (map (flip getJsId st) xs) edges
        where
        edges = if isCons
            then replicate (length xs) JsDataType
            else replicate (length xs-1) JsInput ++ [JsOutput]

mkJsLabel :: JsId -> JsState -> Maybe JsLabel
mkJsLabel jsid st = IMap.lookup (jsNodeId jsid) (jsLabels st)

mkJsParent :: Bool -> JsId -> Parent -> JsState -> Maybe JsParent
mkJsParent isCons jsid (Parent puid (fromEnum -> pidx)) st = case IMap.lookup (jsNodeId jsid) (jsTraceInfo st) of
    Just puid -> Just $ JsParent (getJsId puid st) JsCall
    Nothing -> if puid > 0
        then Just $ JsParent (getJsId puid st) (if isCons then JsDataType else if pidx == len-1 then JsOutput else JsInput)
        else Nothing
 where
    len = maybe 0 length $ IMap.lookup puid (jsChilds st)

    