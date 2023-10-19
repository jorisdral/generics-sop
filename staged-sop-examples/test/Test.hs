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
import GHC.IO (unsafePerformIO)

test :: (NoThunks a, Typeable a) => String -> a -> String
test label x = "unsafeNoThunks (" <> label <> ") = " <> show (unsafeNoThunks x)

main :: IO ()
main = do
    (applyFun -> thunkify) <- generate $ arbitrary @(Fun Int Int)
    mapM_ putStrLn [
        test "Wrap 1            :: Wrap GenericsSOP Int" (Wrap 1            :: Wrap GenericsSOP Int)
      , test "Wrap 1            :: Wrap StagedSOP   Int" (Wrap 1            :: Wrap StagedSOP   Int)
      , test "Wrap (thunkify 1) :: Wrap GenericsSOP Int" (Wrap (thunkify 1) :: Wrap GenericsSOP Int)
      , test "Wrap (thunkify 1) :: Wrap StagedSOP   Int" (Wrap (thunkify 1) :: Wrap StagedSOP   Int)
      ]

-- {-# NOINLINE thunkify #-}
-- thunkify :: a -> a
-- thunkify x = unsafePerformIO (pure $ go 10 x) -- $ go 10
--   where
--     go 0 x = x
--     go n x = go (n-1) x


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

genWithoutThunks :: (Arbitrary a, NFData a) => Gen a
genWithoutThunks = force <$> arbitrary


