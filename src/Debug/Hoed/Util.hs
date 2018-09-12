{-# LANGUAGE StandaloneDeriving, OverloadedStrings, PackageImports #-}

-- | Module containing useful extensions to the Hoed library.
module Debug.Hoed.Util where

import "Hoed" Debug.Hoed
import "Hoed" Debug.Hoed.Observe
import "Hoed" Debug.Hoed.CompTree
import "Hoed" Debug.Hoed.Render
import Data.Graph.Libgraph as LG
import qualified Data.Text as T
import Data.Hashable
import qualified Data.ByteString.Char8 as B8
import  Data.Binary as Binary
import Control.Monad

-- | Prettyprints a Hoed computaation tree stament.
prettyCompStmt :: CompStmt -> String
prettyCompStmt (CompStmt l i (StmtCon c _)) = T.unpack (trimText l) ++ " = " ++ T.unpack (unhashed c)
prettyCompStmt (CompStmt l i (StmtLam args res _)) = sepByStr " " (T.unpack (trimText l):map (T.unpack . unhashed) args) ++ " = " ++ T.unpack (unhashed res)

-- | Prettyprints a Hoed computation tree node.
prettyVertex :: Vertex -> String
prettyVertex RootVertex = "root"
prettyVertex (Vertex s j) = prettyCompStmt s

-- | Prettyprints a @Judgment@.
prettyJudgement :: Judgement -> String
prettyJudgement LG.Right = "right"
prettyJudgement LG.Wrong = "wrong"
prettyJudgement LG.Unassessed = "unassessed"

-- * Additional instances

deriving instance Ord Parent

-- *  Binary serialializations instanaces for Hoed

instance (Binary a,Binary b) => Binary (Graph a b)
instance Binary Judgement
instance Binary AssistedMessage
instance (Binary a,Binary b) => Binary (Arc a b)
instance Binary Vertex
instance Binary CompStmt
instance Binary StmtDetails
instance (Hashable a,Binary a) => Binary (Hashed a) where
    get = liftM hashed Binary.get
    put = Binary.put . unhashed

-- | Utility function to mitigate the debug library HACK (of adding code to observation labels)
trimText :: T.Text -> T.Text
trimText t = w
    where
    ws = T.lines t
    w = if null ws then "" else head ws
    
-- * Libgraph extensions

vertexId :: Vertex -> Int
vertexId RootVertex = 0
vertexId (Vertex s j) = stmtIdentifier s

succArcs :: Eq vertex => Graph vertex arc -> vertex -> [Arc vertex arc]
succArcs g v = filter ((== v) . source) (arcs g)

preorder :: CompTree -> [Vertex]
preorder = getPreorder . getDfs

faultyVertices :: CompTree -> [Vertex]
faultyVertices = findFaulty_dag getJudgement

mapGraphM :: Monad m => (a -> m b) -> Graph a c -> m (Graph b c)
mapGraphM f (Graph r vs as) = do
    r' <- (f r)
    vs' <- (mapM f vs)
    as' <- (mapArcsVM f as)
    return $ Graph r' vs' as'

mapArcsVM :: Monad m => (a -> m b) -> [Arc a c] -> m [Arc b c]
mapArcsVM f = mapM (mapArcVM f)

mapArcVM :: Monad m => (a -> m b) -> Arc a c -> m (Arc b c)
mapArcVM f (Arc src tgt t) = do
    src' <- (f src)
    tgt' <- (f tgt)
    return $ Arc src' tgt' t
    
-- * Miscellaneous

sepByStr :: String -> [String] -> String
sepByStr s [] = []
sepByStr s [x] = x
sepByStr s (x:xs) = x ++ s ++ sepByStr s xs

