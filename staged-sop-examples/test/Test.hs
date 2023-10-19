{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -ddump-splices #-}

module Main (main) where

import Generics.SOP.Staged
import NoThunks.Class
import GHC.Generics
import GHC.Exts.Heap
import ExampleTypes hiding (T)
import Control.DeepSeq
import Text.Printf
import Test.Tasty.QuickCheck
import ExampleFunctions

import Test.Tasty
import Data.Typeable
import GHC.Generics as GHC
import GHC.IO (unsafePerformIO)

test :: (NoThunks a, Typeable a) => String -> a -> String
test label x = "unsafeNoThunks (" <> label <> ") = " <> show (unsafeNoThunks x)

main :: IO ()
main = do
    -- (applyFun -> thunkify) <- generate $ arbitrary @(Fun Int Int)
    mapM_ putStrLn [
        test "Wrap 1            :: Wrap GenericsSOP Int" (Wrap 1            :: Wrap GenericsSOP Int)
      , test "Wrap 1            :: Wrap StagedSOP   Int" (Wrap 1            :: Wrap StagedSOP   Int)
      , test "Wrap 1            :: Wrap Manual      Int" (Wrap 1            :: Wrap Manual      Int)
      , test "Wrap (thunkify 1) :: Wrap GenericsSOP Int" (Wrap (thunkify 1) :: Wrap GenericsSOP Int)
      , test "Wrap (thunkify 1) :: Wrap StagedSOP   Int" (Wrap (thunkify 1) :: Wrap StagedSOP   Int)
      , test "Wrap (thunkify 1) :: Wrap Manual      Int" (Wrap (thunkify 1) :: Wrap Manual      Int)
      ]

{-# NOINLINE thunkify #-}
thunkify :: a -> a
thunkify x = go 10 x -- $ go 10
  where
    go 0 x = if ack 3 3 > 0 then x else x
    go n x = go (n-1) (if ack 3 3 > 0 then x else x)


-- | Ackermann (anything that ghc won't just optimize away..)
ack :: Int -> Int -> Int
ack 0 n = succ n
ack m 0 = ack (pred m) 1
ack m n = ack (pred m) (ack m (pred n))

deriving instance Show a => Show (Wrap tag a)

instance NoThunks a => NoThunks (Tree GenericsSOP a)
instance NoThunks a => NoThunks (Tree StagedSOP a) where
  wNoThunks =
      let
        c = constraints @(Tree StagedSOP a)
      in
        $$(sgwNoThunks' (allC [|| c ||]))


instance NoThunks a => NoThunks (Wrap GenericsSOP a)
instance NoThunks a => NoThunks (Wrap StagedSOP a) where
  wNoThunks =
      let
        c = constraints @(Wrap StagedSOP a)
      in
        $$(sgwNoThunks' (allC [|| c ||]))

instance NoThunks a => NoThunks (Wrap Manual a) where
  wNoThunks ctxt (Wrap a) = noThunks ("Wrap" : ctxt) a
