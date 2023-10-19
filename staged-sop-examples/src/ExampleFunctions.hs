{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
module ExampleFunctions where

import Codec.CBOR.Encoding
import Codec.CBOR.Decoding
import Codec.Serialise
import Control.DeepSeq
import Data.Kind
import Data.SOP
import Data.SOP.Constraint (And, Top)
import Data.Maybe
import Data.SOP.NP
import Data.SOP.NS
import qualified Generics.SOP as SOP
-- import qualified Generics.SOP.NP as SOP
-- import qualified Generics.SOP.NS as SOP
import Generics.SOP.Staged
import GHC.Generics as GHC hiding (C, (:.:))
import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH.Syntax hiding (Type)
import ExampleTypes
import GHC.Exts.Heap
import NoThunks.Class
import GHC.IO (unsafePerformIO)

sgsappend ::
  SIsProductType a xs =>
  NP (C :.: Dict Semigroup) xs ->
  CodeQ a -> CodeQ a -> CodeQ a
sgsappend dicts c1 c2 =
  sproductTypeFrom c1 $ \ a1 -> sproductTypeFrom c2 $ \ a2 ->
    sproductTypeTo
      (zipWith3_NP
        (\ (Comp (C d)) (C a1) (C a2) -> C [|| case $$d of Dict -> ($$a1 <> $$a2) ||])
        dicts a1 a2
      )
-- TODO: it would be nice to simplify this, but it's not clear if it can be done;
-- perhaps by means of quasi-quotation

sgsappend' ::
  SIsProductType a xs =>
  NP (C :.: Dict Semigroup) xs ->
  CodeQ (a -> a -> a)
sgsappend' dicts =
  [|| \ a1 a2 -> $$(sgsappend dicts [|| a1 ||] [|| a2 ||]) ||]

gsappend ::
  (SOP.IsProductType a xs, All Semigroup xs) =>
  a -> a -> a
gsappend a1 a2 =
  SOP.productTypeTo
    (czipWith_NP (Proxy @Semigroup)
      (SOP.mapIII (<>)) (SOP.productTypeFrom a1) (SOP.productTypeFrom a2)
    )

class GSappend a where
  ghcsappend' :: a x -> a x -> a x

instance GSappend U1 where
  ghcsappend' U1 U1 = U1

instance (GSappend a, GSappend b) => GSappend (a :*: b) where
  ghcsappend' (a1 :*: b1) (a2 :*: b2) =
    ghcsappend' a1 a2 :*: ghcsappend' b1 b2

instance Semigroup a => GSappend (K1 r a) where
  ghcsappend' (K1 a1) (K1 a2) = K1 (a1 <> a2)

instance GSappend a => GSappend (M1 i c a) where
  ghcsappend' (M1 a1) (M1 a2) = M1 (ghcsappend' a1 a2)

ghcsappend :: (GHC.Generic a, GSappend (GHC.Rep a)) => a -> a -> a
ghcsappend a1 a2 = to (ghcsappend' (from a1) (from a2))

sgrnf ::
  SGeneric a =>
  POP (C :.: Dict NFData) (SDescription a) ->
  CodeQ a -> CodeQ ()
sgrnf dicts c =
  sfrom c $ \ a ->
    foldr (\ x r -> [|| $$x `seq` $$r ||]) [|| () ||]
      (collapse_SOP (selectWith_SOP (\ (Comp (C d)) (C x) -> K [|| withDict $$d (rnf $$x) ||]) dicts a))

gShowEnum ::
  SOP.IsEnumType a => NP (K String) (SOP.Code a) -> a -> String
gShowEnum names a =
  collapse_NS (SOP.hzipWith const names (SOP.enumTypeFrom a))

sgShowEnum ::
  SIsEnumType a => NP (K String) (SDescription a) -> CodeQ a -> CodeQ String
sgShowEnum names c =
  senumTypeFrom c $ \ a ->
    liftTyped (collapse_NS (selectWith_NS const names a))

s15Names :: NP (K String) (SOP.Code S15)
s15Names =
  K "1" :* K "2" :* K "3" :* K "4" :* K "5" :* K "6" :* K "7" :* K "8" :* K "9" :* K "10" :* K "11" :* K "12" :* K "13" :* K "14" :* K "15" :* Nil

geq ::
  (SOP.Generic a, SOP.All (SOP.All Eq) (SOP.Code a)) =>
  a -> a -> Bool
geq a1 a2 =
  SOP.ccompare_SOP (Proxy @Eq)
    False
    (\ xs1 xs2 -> and (collapse_NP (czipWith_NP (Proxy @Eq) (SOP.mapIIK (==)) xs1 xs2)))
    False
    (SOP.from a1) (SOP.from a2)

-- This transcription is quite ugly and annoying, because compare_SOP does not exist
-- with an extra argument ...
sgeq ::
  SGeneric a =>
  POP (C :.: Dict Eq) (SDescription a) ->
  CodeQ a -> CodeQ a -> CodeQ Bool
sgeq dicts c1 c2 =
  sfrom c1 $ \ a1 -> sfrom c2 $ \ a2 ->
  ccompare_SOP (Proxy @Top)
    ([|| False ||])
    (\ xs1 xs2 -> sand (collapse_NP (zipWith_NP (\ (P (Comp (C d)) (C x1)) (C x2) -> K [|| withDict $$d ($$x1 == $$x2) ||]) xs1 xs2)))
    ([|| False ||])
    (selectWith_SOP P dicts a1) a2

data P (f :: k -> Type) (g :: k -> Type) (x :: k) = P (f x) (g x)

sand :: [CodeQ Bool] -> CodeQ Bool
sand = foldr (\ x r -> [|| $$x && $$r ||]) [|| True ||]

genum :: (SOP.Generic a, SOP.All ((~) '[]) (SOP.Code a)) => [a]
genum =
  SOP.to <$> SOP.apInjs_POP (POP (cpure_NP (Proxy @((~) '[])) Nil))

sgenum :: SIsEnumType a => CodeQ [a]
sgenum =
  foldr (\ x r -> [|| $$x : $$r ||]) [|| [] ||]
    (sto <$> apInjs_POP (POP (cpure_NP (Proxy @((~) '[])) Nil)))

class GEq a where
  ghcgeq' :: a x -> a x -> Bool

instance GEq U1 where
  ghcgeq' U1 U1 = True

instance (GEq a, GEq b) => GEq (a :*: b) where
  ghcgeq' (a1 :*: b1) (a2 :*: b2) =
    ghcgeq' a1 a2 && ghcgeq' b1 b2

instance (GEq a, GEq b) => GEq (a :+: b) where
  ghcgeq' (L1 a1) (L1 a2) = ghcgeq' a1 a2
  ghcgeq' (R1 b1) (R1 b2) = ghcgeq' b1 b2
  ghcgeq' _ _ = False

instance Eq a => GEq (K1 r a) where
  ghcgeq' (K1 a1) (K1 a2) = a1 == a2

instance GEq a => GEq (M1 i c a) where
  ghcgeq' (M1 a1) (M1 a2) = ghcgeq' a1 a2

ghcgeq :: (GHC.Generic a, GEq (Rep a)) => a -> a -> Bool
ghcgeq a1 a2 = ghcgeq' (from a1) (from a2)

conNumbers :: forall a . (SOP.Generic a) => Proxy a -> NP (K Word) (SOP.Code a)
conNumbers _ =
  ana_NP (\ (K i) -> (K i, K (i + 1))) (K 0)

sconNumbers :: forall a . (SGeneric a) => Proxy a -> NP (K Word) (SDescription a)
sconNumbers _ =
  ana_NP (\ (K i) -> (K i, K (i + 1))) (K 0)

conArities :: forall a . (SOP.Generic a) => Proxy a -> NP (K Word) (SOP.Code a)
conArities _ =
  let
    go :: forall xs . SOP.SListI xs => K Word xs
    go = K (fromIntegral (SOP.lengthSList (Proxy @xs)))
  in
    cpure_NP (Proxy @SOP.SListI) go

sconArities :: forall a . (SGeneric a) => Proxy a -> NP (K Word) (SDescription a)
sconArities _ =
  let
    go :: forall xs . SListI xs => K Word xs
    go = K (fromIntegral (lengthSList (Proxy @xs)))
  in
    cpure_NP (Proxy @SListI) go

gencode :: forall a . (SOP.Generic a, SOP.All (SOP.All Serialise) (SOP.Code a)) => a -> Encoding
gencode x =
  let
    tmp :: SOP (K Encoding) (SOP.Code a)
    tmp =
      cmap_SOP (Proxy @Serialise) (SOP.mapIK encode) (SOP.from x)

    tmp2 :: NS (K Encoding) (SOP.Code a)
    tmp2 =
      SOP.hzipWith3
        (\ (K i) (K a) es -> K (encodeListLen (a + 1) <> encodeWord i <> mconcat (collapse_NP es)))
        (conNumbers (Proxy @a))
        (conArities (Proxy @a))
        (SOP.unSOP tmp)
  in
    collapse_NS tmp2

sgencode :: forall a . SGeneric a => POP (C :.: Dict Serialise) (SDescription a) -> CodeQ (a -> Encoding)
sgencode dicts =
  [||
    \ a ->
    $$(sfrom [|| a ||] $ \ a' ->
      let
        tmp1 :: SOP (K (CodeQ Encoding)) (SDescription a)
        tmp1 =
          selectWith_SOP (\ (Comp (C d)) (C a) -> K [|| withDict $$d (encode $$a) ||]) dicts a'

        tmp2 :: forall a . SGeneric a => Proxy a -> NP (K (Word, Word)) (SDescription a)
        tmp2 _ =
          zipWith_NP
            (\ (K cn) (K ca) -> K (cn, ca))
            (sconNumbers (Proxy @a)) (sconArities (Proxy @a))

        tmp3 :: NS (K (CodeQ Encoding)) (SDescription a)
        tmp3 =
          selectWith_NS
            (\ (K (i, a)) es ->
              let
                a' = a + 1
              in
                K [||
                     encodeListLen a'
                  <> encodeWord i
                  <> $$(foldr (\ x y -> [|| $$x <> $$y ||]) [|| mempty ||] (collapse_NP es))
                ||]
            )
            (tmp2 (Proxy @a))
            (unSOP tmp1)
      in
        collapse_NS tmp3
    )
  ||]

gdecode :: forall a s . (SOP.Generic a, All (All Serialise) (SOP.Code a)) => Decoder s a
gdecode =
  let
    tmp :: POP (Decoder s) (SOP.Code a)
    tmp =
      cpure_POP (Proxy @Serialise) decode

    tmp2 :: NP (K (SOP (Decoder s) (SOP.Code a))) (SOP.Code a)
    tmp2 =
      apInjs'_POP tmp

    tmp3 :: NP (K ((Word, Word), Decoder s a)) (SOP.Code a)
    tmp3 =
      zipWith3_NP
        (\ (K i) (K a) (K dec) -> K ((a + 1, i), SOP.to <$> sequence_SOP dec))
        (conNumbers (Proxy @a))
        (conArities (Proxy @a))
        tmp2
  in
    do
      len <- fromIntegral <$> decodeListLen
      tag <- decodeWord
      case lookup (len, tag) (collapse_NP tmp3) of
        Just dec -> dec
        Nothing  -> fail "invalid encoding"

dApplicativeDecoder :: Dict Applicative (Decoder s)
dApplicativeDecoder = Dict

sgdecode :: forall a s . SGeneric a => POP (C :.: Dict Serialise) (SDescription a) -> CodeQ (Decoder s a)
sgdecode dicts =
  let
    tmp :: POP (C :.: Decoder s) (SDescription a)
    tmp =
      map_POP (\ (Comp (C d)) -> Comp (C [|| withDict $$d decode ||])) dicts

    tmp2 :: NP (K (SOP (C :.: Decoder s) (SDescription a))) (SDescription a)
    tmp2 =
      apInjs'_POP tmp

    tmp3 :: NP (K ((Word, Word), CodeQ (Decoder s a))) (SDescription a)
    tmp3 =
      zipWith_NP
        (\ (K (i, a)) (K dec) -> K ((a + 1, i), stoA [|| dApplicativeDecoder ||] dec))
        (tmp3' (Proxy @a))
        tmp2

    tmp3' :: forall a . SGeneric a => Proxy a -> NP (K (Word, Word)) (SDescription a)
    tmp3' _ =
      zipWith_NP (SOP.mapKKK (,)) (sconNumbers (Proxy @a)) (sconArities (Proxy @a))

    tmp4 :: [((Word, Word), CodeQ (Decoder s a))]
    tmp4 =
      collapse_NP tmp3

    tmp5 :: CodeQ (Word, Word) -> [((Word, Word), CodeQ (Decoder s a))] -> CodeQ (Decoder s a)
    tmp5 _sym []                  = [|| fail "invalid encoding" ||]
    tmp5 sym ((key, rhs) : cases) =
      [|| if $$sym == key then $$rhs else $$(tmp5 sym cases) ||]
  in
    [||
      do
        len <- fromIntegral <$> decodeListLen
        tag <- decodeWord
        $$(tmp5 [|| (len, tag) ||] tmp4)
    ||]

sid :: SGeneric a => CodeQ a -> CodeQ a
sid x = sfrom x sto

sid2 x = sto x


sgwNoThunks' ::
     forall a. SGeneric a
  => POP (C :.: Dict NoThunks) (SDescription a)
  -> CodeQ (Context -> a -> IO (Maybe ThunkInfo))
sgwNoThunks' dicts = [|| \ctxt x -> $$(sgwNoThunks dicts [|| ctxt ||] [|| x ||]) ||]

sgwNoThunks ::
     forall a. SGeneric a
  => POP (C :.: Dict NoThunks) (SDescription a)
  -> CodeQ Context -> CodeQ a -> CodeQ (IO (Maybe ThunkInfo))
sgwNoThunks dicts ctxt c = sfrom c $ \a ->
    foldr (\x r -> [|| $$x >>= maybe $$r (pure . Just) ||]) [|| pure Nothing ||] (f a)
  where
    f :: SRep a -> [CodeQ (IO (Maybe ThunkInfo))]
    f a = collapse_SOP $ selectWith_SOP g dicts a

    g :: forall b. (C :.: Dict NoThunks) b -> C b -> K (CodeQ (IO (Maybe ThunkInfo))) b
    g (Comp (C d)) (C x) = K [|| withDict $$d (noThunks (updctxt $$ctxt $$d) $$x) ||]

updctxt :: NoThunks a => Context -> proxy a -> Context
updctxt ctxt p = case ctxt of
    hd : tl | hd == showTypeOf (toProxy p) -> tl
    _otherwise                             -> ctxt

toProxy :: proxy a -> Proxy a
toProxy _ = Proxy
