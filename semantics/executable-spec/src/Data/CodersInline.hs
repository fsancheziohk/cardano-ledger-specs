{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

-- | This module is an extension to Data.Coders that provides the same capabilities,
--   except it uses inlineable functions, rather a set of recursive functions over a GADT.
--   GHC will inline these functions (where it will not inline a recursive function) so
--   these may be considerably faster. The only downside is that the structure of the
--   Coding is not an analyzable data structure, so it is hard to add extra functionality.
--   These combinators have exactly the same types as the GADT constructor functions, so
--   the benefits of the strong typing are retained.

module Data.CodersInline
  ( FromCBOR (fromCBOR),
    ToCBOR (toCBOR),
    Decoder,
    Encoding,
    Dual(..),
    ILField(..),
    fieldD,
    Density(..),
    Wrapped (..),
    ILEncode(..),
    recE,
    sumE,
    keyedE,
    toE,
    encE,
    (|>),
    dualE,
    omitCE,
    omitE,
    keyE,
    encodeE,
    ILDecode(..),
    sumD,
    recD,
    keyedD,
    fromD,
    decD,
    dualD,
    (<|),
    invalidD,
    mapD,
    emitD,
    annD,
    (<*|),
    summandsD,
    sparseKeyedD,
    decodeD,
  )
  where

import Cardano.Binary
  ( FromCBOR (fromCBOR),
    ToCBOR (toCBOR),
    encodeListLen,
    encodeMapLen,
    encodeWord,
    decodeWord,
    decodeBreakOr,
    decodeMapLenOrIndef,
  )
import Data.Coders
import Data.Typeable
import Data.Set(Set,empty,member,insert)


-- ===============================================================================
-- The inlineable Encode is 4-tuple corresponding to the range of the 4 recursive
-- functions over Encode:  runE, gsize, encodeCountPrefix, and encodeClosed

data ILEncode (w::Wrapped) t where
  ILEncode :: !t -> !Word -> !(Word -> Encoding) -> Encoding -> ILEncode w t

recE :: t -> ILEncode ('Closed 'Dense) t
recE con = ILEncode con 0 (\ n -> if n==0 then mempty else encodeListLen n) mempty
{-# INLINE recE #-}

sumE :: t -> Word -> ILEncode 'Open t
sumE con tag = ILEncode con 0 (\ n -> encodeListLen (n + 1) <> encodeWord tag) mempty
{-# INLINE sumE #-}

keyedE :: t -> ILEncode ('Closed 'Sparse) t
keyedE con = ILEncode con 0 (\ n -> encodeMapLen n) mempty
{-# INLINE keyedE #-}

toE :: ToCBOR a => a -> ILEncode ('Closed 'Dense) a
toE x = ILEncode x 1 (\ _ -> toCBOR x) (toCBOR x)
{-# INLINE toE #-}

encE :: (t -> Encoding) -> t -> ILEncode ('Closed 'Dense) t
encE enc x = ILEncode x 1 (\ _ -> enc x) (enc x)
{-# INLINE encE #-}

infixl 4 |>
(|>) :: ILEncode w (a -> t) -> ILEncode ('Closed r) a -> ILEncode w t
(|>) (ILEncode f i ec1 e1) (ILEncode a j _ec2 e2) = ILEncode (f a) (i+j) (\ n -> ec1 (n+j) <> e2) (e1 <> e2)
{-# INLINE (|>) #-}

dualE :: Dual t -> t -> ILEncode ('Closed 'Dense) t
dualE (Dual enc _dec) x = ILEncode x 1 (\ _ -> enc x) (enc x)
{-# INLINE dualE #-}

omitCE :: t -> ILEncode w t
omitCE x = ILEncode x 0 (\ _ -> mempty) mempty
{-# INLINE omitCE #-}

omitE:: (t -> Bool) -> ILEncode ('Closed 'Sparse) t -> ILEncode ('Closed 'Sparse) t
omitE p (ILEncode t i enc e) =
  ILEncode
   t
   (if p t then 0 else i)
   (if p t then (\ _ -> mempty) else enc)
   (if p t then mempty else e)

{-# INLINE omitE #-}

keyE :: Word -> ILEncode ('Closed 'Dense) t -> ILEncode ('Closed 'Sparse) t
keyE key (ILEncode t i enc e) = ILEncode t i (\ n -> encodeWord key <> enc n)  (encodeWord key <> e)
{-# INLINE keyE #-}

encodeE :: ILEncode w t -> Encoding
encodeE (ILEncode _t _i enc _e) = enc 0
{-# INLINE encodeE #-}

-- ===============================================================================
-- The inlineable Decode is 3-tuple corresponding to the range of the 3 recursive
-- functions over Decode:   hsize, decodeCount, and decodeClosed

data ILDecode (w::Wrapped) t where
  ILDecode :: !Int -> !(forall s. Int -> Decoder s (Int, t)) -> !(forall s. Decoder s t) -> ILDecode w t

sumD :: t -> ILDecode 'Open t
sumD cn = ILDecode 0 (\ n -> pure(n+1,cn)) (pure cn)
{-# INLINE sumD #-}

recD :: t -> ILDecode ('Closed 'Dense) t
recD cn = ILDecode 0 (\ n -> decodeRecordNamed "RecD" (const n) (pure (n, cn))) (pure cn)
{-# INLINE recD #-}

keyedD :: t -> ILDecode ('Closed 'Sparse) t
keyedD cn = ILDecode 0 (\ n -> pure(n+1,cn)) (pure cn)
{-# INLINE keyedD #-}

fromD :: FromCBOR t => ILDecode w t
fromD = ILDecode 1 (\ n -> (n,) <$> fromCBOR) (fromCBOR)
{-# INLINE fromD #-}

decD :: (forall s. Decoder s t) -> ILDecode ('Closed 'Dense) t
decD dec = ILDecode 1 (\n -> (n,) <$> dec) dec
{-# INLINE decD #-}

dualD :: Dual t -> ILDecode ('Closed 'Dense) t
dualD (Dual _ dec) = ILDecode 1 (\n -> (n,) <$> dec) dec
{-# INLINE dualD #-}

infixl 4 <|
(<|) :: ILDecode w1 (a -> t) -> ILDecode ('Closed d) a -> ILDecode w1 t
(<|) (ILDecode i f1 d1) (ILDecode j _f2 d2) =
     ILDecode (i+j) (\ n -> do (k,f) <- f1 (n + j); y <- d2; pure(k, f y))
              (do f <- d1; y <- d2; pure (f y))
{-# INLINE (<|) #-}

invalidD :: Word -> ILDecode w t
invalidD k = ILDecode 0 (\ _ -> invalidKey k) (invalidKey k)
{-# INLINE invalidD #-}

mapD :: (a -> b) -> ILDecode w a -> ILDecode w b
mapD f (ILDecode i g d) = ILDecode i (\ n -> do (j,x) <- g n; pure(j, f x)) (f <$> d)
{-# INLINE mapD #-}

emitD :: t -> ILDecode w t
emitD x = ILDecode 0 (\ n -> pure(n,x)) (pure x)
{-# INLINE emitD #-}

annD :: ILDecode w t -> ILDecode w (Annotator t)
annD (ILDecode i g d) = ILDecode i (\ n -> do (m,y) <- g n; pure(m,pure y)) (fmap pure d)
{-# INLINE annD #-}

infixl 4 <*|
(<*|) :: ILDecode w1 (Annotator(a -> t)) -> ILDecode('Closed d) (Annotator a) -> ILDecode w1 (Annotator t)
(<*|) (ILDecode i f1 d1) (ILDecode j _f2 d2) =
      ILDecode (i+j) (\ n -> do (k,f) <- f1 (n + j); y <- d2; pure(k, f <*> y))
               (do f <- d1; y <- d2; pure (f <*> y))
{-# INLINE (<*|) #-}

summandsD :: String -> (Word -> ILDecode 'Open t) -> ILDecode ('Closed 'Dense) t
summandsD nm f =
  ILDecode 1
     (\n -> (n + 1,) <$> decodeRecordSum nm (\x -> case (f x) of (ILDecode _ g _) -> g 0))
     (decodeRecordSum nm (\x -> case (f x) of (ILDecode _ g _) -> g 0))
{-# INLINE summandsD #-}

sparseKeyedD :: Typeable t => String -> t -> (Word -> ILField t) -> [(Word,String)] -> ILDecode ('Closed 'Dense) t
sparseKeyedD name initial pick required =
  ILDecode 1 (\ n -> (n+1,) <$> decSparse  name initial pick required)
       (decSparse name initial pick required)
{-# INLINE sparseKeyedD #-}

decodeD :: ILDecode w t -> Decoder s t
decodeD (ILDecode _ g _) = fmap snd (g 0)
{-# INLINE decodeD #-}

-- ========================================================

fieldD :: (x -> t -> t) -> ILDecode ('Closed d) x -> Field t
fieldD update ildecode = Field update (D (decodeD ildecode))

data ILField t where
  ILField:: (x -> t -> t) -> ILDecode ('Closed d) x -> ILField t

-- =========================================================
-- Show instance

instance Show t => Show (ILEncode w t) where
  show (ILEncode t w enc _e) = "(ILEncode "++show t++" "++show w++"\n  "++show(enc 0)++")"
    -- if we try and print the _e field, and we have used omitE, we will get an error,
    -- because the (omitE p enc) the encoding will pull on enc even when p is true when
    -- we print.


getSparseBlock2 :: Maybe Int -> t -> (Set Word -> Decoder s (t -> t,Set Word)) -> Set Word -> Decoder s (t,Set Word)
getSparseBlock2 (Just 0) initial _getOneItem seen = pure(initial,seen)
getSparseBlock2 (Just n) initial getOneItem seen = do
   (transform,seen2) <- getOneItem seen
   getSparseBlock2 (Just(n-1)) (transform initial) getOneItem seen2
getSparseBlock2 Nothing initial getOneItem seen =
   decodeBreakOr >>= \case
             True -> pure(initial,seen)
             False -> do (transform,seen2) <-getOneItem seen
                         getSparseBlock2 Nothing (transform initial) getOneItem seen2

decSparse ::
   Typeable a =>
   String -> a -> (Word -> ILField a) -> [(Word, String)] -> Decoder s a
decSparse name initial pick required = do
  lenOrIndef <- decodeMapLenOrIndef
  (!v,used) <- getSparseBlock2 lenOrIndef initial (getField name pick) empty
  if all (\ (key,_name) -> member key used) required
     then pure v
     else unusedRequiredKeys used required (show(typeOf initial))

getField :: String -> (Word -> ILField t) -> Set Word -> Decoder s (t -> t,Set Word)
getField name f seen = do
  tag <- decodeWord
  if member tag seen
     then duplicateKey name tag
     else case f tag of
            ILField update d -> do v <- decodeD d; pure (update v,insert tag seen)
