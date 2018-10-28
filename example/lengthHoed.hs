{-# LANGUAGE ExtendedDefaultRules, FlexibleContexts, TemplateHaskell, ViewPatterns, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module LengthHoed(main, mylength) where
import Debug.Hoed.Graphical

debug [d|

    mylength :: [a] -> Int
    mylength [] = 0
    mylength (x:xs) = succ (mylength xs)
    
   |]

main = debugGraphical $ print $ mylength "hs"
