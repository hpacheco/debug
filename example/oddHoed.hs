{-# LANGUAGE ExtendedDefaultRules, FlexibleContexts, TemplateHaskell, ViewPatterns, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module QuickSortHoed(main, oddNat,evenNat) where
import Debug.Hoed.Algorithmic

debug [d|

    oddNat :: Int -> Bool
    oddNat 0 = False
    oddNat 1 = True
    oddNat n = evenNat (pred n)

    evenNat :: Int -> Bool
    evenNat 0 = True
    evenNat 1 = False
    evenNat n = oddNat (pred n)

   |]

main = debugRun $ print $ oddNat 5
