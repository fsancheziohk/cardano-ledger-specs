{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- =========================

module Test.Cardano.Ledger.ShelleyMA.TxBody (txBodyTest, TestEra) where

import Cardano.Ledger.Core (Script, TxBody, Value)
import qualified Cardano.Ledger.Mary.Value ()
import qualified Cardano.Ledger.Mary.Value as ConcreteValue
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import qualified Cardano.Ledger.ShelleyMA.TxBody as Mary
import Cardano.Ledger.Val (Val (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import qualified Data.Map.Strict as Map
import Data.MemoBytes (MemoBytes (Memo), roundTripMemo)
import Data.Sequence.Strict (StrictSeq, fromList)
import Data.Set (empty)
import GHC.Records
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (SJust, SNothing))
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.TxBody (Wdrl (..))
import qualified Shelley.Spec.Ledger.TxBody as Shelley
import Test.Tasty
import Test.Tasty.HUnit
import Test.Cardano.Ledger.ShelleyMA.TestEra(TestEra,TestScript)

import Cardano.Ledger.ShelleyMA.TxBody(TxBody'(..),FamsFrom,FamsTo)
import Data.CodersInline
import Data.Coders(Encode(..),encode,(!>),Decode(..),decode,Field(..),
      decodeSet,decodeStrictSeq,encodeFoldable)

import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators() -- Arbitrary instances
import Test.Shelley.Spec.Ledger.Serialisation.Generators() -- Arbitrary instances
import Test.Tasty.QuickCheck
-- import Debug.Trace(trace)
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Coders(roundTrip',embedTrip,embedTrip')
-- ============================================================================================
-- make an example
-- ============================================================================================

-- First make a fully concrete Era where the Hashing is concrete
-- without this we won't be able to Serialize or Hash TxID. We use
--the tools supplied by Test.Cardano.Ledger.ShelleyMA.TestEra(TestEra,TestScript)

type instance Value TestEra = ConcreteValue.Value TestEra
type instance Script TestEra = TestScript
type instance TxBody TestEra = Mary.TxBody TestEra

-- ====================================================================================================
-- Make a TxBody to test with

eseq :: StrictSeq a
eseq = fromList []

txM :: Mary.TxBody TestEra
txM =
  Mary.TxBody
    empty
    eseq
    eseq
    (Wdrl Map.empty)
    (Coin 6)
    (ValidityInterval (SJust (SlotNo 3)) (SJust (SlotNo 42)))
    SNothing
    SNothing
    (inject (Coin 2))

bytes :: Mary.TxBody era -> ShortByteString
bytes (Mary.STxBody (Memo _ b)) = b

fieldTests :: TestTree
fieldTests =
  testGroup
    "getField tests"
    [ testCase "inputs" (assertEqual "inputs" (getField @"inputs" txM) empty),
      testCase "outputs" (assertEqual "outputs" (getField @"outputs" txM) eseq),
      testCase "certs" (assertEqual "certs" (getField @"certs" txM) eseq),
      testCase "wdrls" (assertEqual "wdrls" (getField @"wdrls" txM) (Wdrl Map.empty)),
      testCase "txfree" (assertEqual "txfree" (getField @"txfee" txM) (Coin 6)),
      testCase "vldt" (assertEqual "vldt" (getField @"vldt" txM) (ValidityInterval (SJust (SlotNo 3)) (SJust (SlotNo 42)))),
      testCase "update" (assertEqual "update" (getField @"update" txM) SNothing),
      testCase "mdHash" (assertEqual "mdHash" (getField @"mdHash" txM) SNothing),
      testCase "forge" (assertEqual "forge" (getField @"forge" txM) (inject (Coin 2)))
    ]

roundtrip :: Mary.TxBody TestEra -> Bool
roundtrip (Mary.STxBody memo) =
  case roundTripMemo memo of
    Right ("", new) -> new == memo
    _other -> False

txBodyTest :: TestTree
txBodyTest =
  testGroup
    "TxBody"
    [ fieldTests,
      testCase "length" (assertEqual "length" (Short.length (bytes txM)) 19),
      testCase "roundtrip" (assertBool "rountrip" (roundtrip txM))
    ]


-- =======================================================================
-- New and better coders that use SparseEncoding


encVI :: ValidityInterval -> ILEncode ( 'Closed 'Dense) ValidityInterval
encVI (ValidityInterval f t) = recE ValidityInterval |> toE f |> toE t

decVI :: ILDecode ( 'Closed 'Dense) ValidityInterval
decVI = recD ValidityInterval <| fromD <| fromD

decTxBody:: (FamsFrom era) => ILDecode ('Closed 'Dense) (TxBody' era)
decTxBody =
      recD TxBody'
        <| fromD
        <| decD (decodeStrictSeq fromCBOR)
        <| decD (decodeStrictSeq fromCBOR)
        <| fromD
        <| fromD
        <| decVI -- CBOR Group decoding
        <| fromD
        <| fromD
        <| fromD

encTxBody:: FamsTo era => TxBody' era -> ILEncode ('Closed 'Dense) (TxBody' era)
encTxBody (TxBody' i o d w fee vi u m frg) =
          recE TxBody'
            |> toE i
            |> encE encodeFoldable o
            |> encE encodeFoldable d
            |> toE w
            |> toE fee
            |> (encVI vi) -- CBOR Group encoding
            |> toE u
            |> toE m
            |> toE frg

-- ==============================================
-- Sparse coders for StrictMaybe types

isSNothing :: StrictMaybe a -> Bool
isSNothing SNothing = True
isSNothing _ = False

fromSJust :: StrictMaybe a -> a
fromSJust (SJust x) = x
fromSJust SNothing = error "SNothing in fromSJust"

encodeKeyedStrictMaybe :: ToCBOR a =>  Word -> StrictMaybe a -> Encode ('Closed 'Sparse) (StrictMaybe a)
encodeKeyedStrictMaybe key x = Omit isSNothing (Key key (E (toCBOR . fromSJust) x))

txSparse :: (Val (Value era),FamsTo era) => TxBody' era -> Encode ( 'Closed 'Sparse) (TxBody' era)
txSparse (TxBody' inp out cert wdrl fee (ValidityInterval bot top) up hash frge) =
  Keyed (\i o f topx c w u h botx forg -> TxBody' i o c w f (ValidityInterval botx topx) u h forg)
    !> Key 0 (E encodeFoldable inp) -- We don't have to send these in TxBodyX order
    !> Key 1 (E encodeFoldable out) -- Just hack up a fake constructor with the lambda.
    !> Key 2 (To fee)
    !> encodeKeyedStrictMaybe 3 top
    !> Omit null (Key 4 (E encodeFoldable cert))
    !> Omit (null . unWdrl) (Key 5 (To wdrl))
    !> encodeKeyedStrictMaybe 6 up
    !> encodeKeyedStrictMaybe 7 hash
    !> encodeKeyedStrictMaybe 8 bot
    !> Omit isZero (Key 9 (To frge))

bodyFields :: FamsFrom era => Word -> Field (TxBody' era)
bodyFields 0 = Field (\x tx -> tx {inputs = x}) (D (decodeSet fromCBOR))
bodyFields 1 = Field (\x tx -> tx {outputs = x}) (D (decodeStrictSeq fromCBOR))
bodyFields 2 = Field (\x tx -> tx {txfee = x}) From
bodyFields 3 = Field (\x tx -> tx {vldt = (vldt tx){validTo = x}}) (D (SJust <$> fromCBOR))
bodyFields 4 = Field (\x tx -> tx {certs = x}) (D (decodeStrictSeq fromCBOR))
bodyFields 5 = Field (\x tx -> tx {wdrls = x}) From
bodyFields 6 = Field (\x tx -> tx {update = x}) (D (SJust <$> fromCBOR))
bodyFields 7 = Field (\x tx -> tx {mdHash = x}) (D (SJust <$> fromCBOR))
bodyFields 8 = Field (\x tx -> tx {vldt = (vldt tx){validFrom = x}}) (D (SJust <$> fromCBOR))
bodyFields 9 = Field (\x tx -> tx {forge = x}) From
bodyFields n = Field (\_ t -> t) (Invalid n)

getTxSparse ::  (Val (Value era),FamsFrom era) => Decode ('Closed 'Dense) (TxBody' era)
getTxSparse = SparseKeyed "TxBody'" initial bodyFields [(0,"inputs"),(1,"outputs"),(2,"txfee")]

baz x = roundTrip' (encode . txSparse) (decode getTxSparse) x

checkSparse :: TxBody' TestEra -> Bool
checkSparse tx = case baz tx of
    Right("",_) -> True
    Right(left,_) -> error ("left over input: "++show left)
    Left s -> error (show s)

-- ===============================================
-- inlineable encoders and decoders

encodeKeyedStrictMaybe2 :: ToCBOR a =>  Word -> StrictMaybe a -> ILEncode ('Closed 'Sparse) (StrictMaybe a)
encodeKeyedStrictMaybe2 key x =
 omitE isSNothing (keyE key (encE (toCBOR . fromSJust) x))

txSparse2 :: (Val (Value era),FamsTo era) => TxBody' era -> ILEncode ( 'Closed 'Sparse) (TxBody' era)
txSparse2 (TxBody' inp out cert wdrl fee (ValidityInterval bot top) up hash frge) =
  keyedE (\i o f topx c w u h botx forg -> TxBody' i o c w f (ValidityInterval botx topx) u h forg)
    |> keyE 0 (encE encodeFoldable inp) -- We don't have to send these in TxBodyX order
    |> keyE 1 (encE encodeFoldable out) -- Just hack up a fake constructor with the lambda.
    |> keyE 2 (toE fee)
    |> encodeKeyedStrictMaybe2 3 top
    |> omitE null (keyE 4 (encE encodeFoldable cert))
    |> omitE (null . unWdrl) (keyE 5 (toE wdrl))
    |> encodeKeyedStrictMaybe2 6 up
    |> encodeKeyedStrictMaybe2 7 hash
    |> encodeKeyedStrictMaybe2 8 bot
    |> omitE isZero (keyE 9 (toE frge))

bodyFields2 :: FamsFrom era => Word -> ILField (TxBody' era)
bodyFields2 0 = ILField (\x tx -> tx {inputs = x}) (decD (decodeSet fromCBOR))
bodyFields2 1 = ILField (\x tx -> tx {outputs = x}) (decD (decodeStrictSeq fromCBOR))
bodyFields2 2 = ILField (\x tx -> tx {txfee = x}) fromD
bodyFields2 3 = ILField (\x tx -> tx {vldt = (vldt tx){validTo = x}}) (decD (SJust <$> fromCBOR))
bodyFields2 4 = ILField (\x tx -> tx {certs = x}) (decD (decodeStrictSeq fromCBOR))
bodyFields2 5 = ILField (\x tx -> tx {wdrls = x}) fromD
bodyFields2 6 = ILField (\x tx -> tx {update = x}) (decD (SJust <$> fromCBOR))
bodyFields2 7 = ILField (\x tx -> tx {mdHash = x}) (decD (SJust <$> fromCBOR))
bodyFields2 8 = ILField (\x tx -> tx {vldt = (vldt tx){validFrom = x}}) (decD (SJust <$> fromCBOR))
bodyFields2 9 = ILField (\x tx -> tx {forge = x}) fromD
bodyFields2 n = ILField (\_ t -> t) (invalidD n)

getTxSparse2 ::  (Val (Value era),FamsFrom era) => ILDecode ('Closed 'Dense) (TxBody' era)
getTxSparse2 = sparseKeyedD "TxBody'" initial bodyFields2 [(0,"inputs"),(1,"outputs"),(2,"txfee")]

baz2 x = roundTrip' (encodeE . txSparse2) (decodeD getTxSparse2) x

checkSparse2 :: TxBody' TestEra -> Bool
checkSparse2 tx = case baz2 tx of
    Right("",_) -> True
    Right(left,_) -> error ("left over input: "++show left)
    Left s -> error (show s)

initial :: (Val (Value era)) => TxBody' era
initial =
  TxBody'
    empty
    eseq
    eseq
    (Wdrl Map.empty)
    (Coin 0)
    (ValidityInterval SNothing SNothing)
    SNothing
    SNothing
    zero


genShelleyBody :: Gen (Shelley.TxBody TestEra)
genShelleyBody = Shelley.TxBody <$> arbitrary <*> pure eseq <*> arbitrary <*> arbitrary
                                <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

test = do
  shelleybody <- genShelleyBody
  case embedTrip' toCBOR (decodeD (getTxSparse2 @TestEra)) shelleybody of
     Right("",_) -> pure True
     Right(left,_) -> error ("left over input: "++show left)
     Left s -> error (show s)

instance Arbitrary (TxBody' TestEra) where
  arbitrary = do
    ins <- arbitrary
    f <- arbitrary
    c <- arbitrary
    w <- arbitrary
    u <- arbitrary
    m <- arbitrary
    v <- arbitrary
    frg <- arbitrary
    pure (initial{ inputs = ins
                 , txfee = f
                 , certs = c
                 , wdrls = w
                 , update = u
                 , mdHash = m
                 , vldt = v
                 , forge = frg
                 } )

main = do
  tx <- generate (arbitrary :: Gen(TxBody' TestEra))
  case (baz tx) of
    Right("",_) -> putStrLn "OK"
    Right(left,_) -> putStrLn ("left over input: "++show left)
    Left s -> putStrLn(show s)



sparseTest = testGroup "sparse coding"
   [ testProperty "roundtrip sparse TxBody'" checkSparse
   , testProperty "roundtrip inline sparse TxBody'" checkSparse2
   , testProperty "embed Shelley" test
   ]

-- ==============================================================
-- An example

tx2 :: TxBody' TestEra
tx2 =
  TxBody'
    empty
    eseq
    eseq
    (Wdrl Map.empty)
    (Coin 6)
    (ValidityInterval (SJust (SlotNo 3)) (SJust (SlotNo 42)))
    SNothing
    SNothing
    (inject (Coin 2))

vali =  (ValidityInterval (SJust (SlotNo 3)) (SJust (SlotNo 42)))

foo x = roundTrip' (encodeE . encTxBody) (decodeD decTxBody) x

bar x = roundTrip' (encodeE . encVI) (decodeD decVI) x

{-
omitStrictNothingDual :: (FromCBOR t, ToCBOR t) => Dual (StrictMaybe t)
omitStrictNothingDual = Dual (toCBOR . fromJust . strictMaybeToMaybe) (SJust <$> fromCBOR)



-- | Choose a de-serialiser when given the key (of type Word).
--   Wrap it in a Field which pairs it with its update function which
--   changes only the field being deserialised.
boxBody :: ProperFrom era => Word -> Field (TxBodyX era)
boxBody 0 = Field (\x tx -> tx {_inputsX = x}) (D (decodeSet fromCBOR))
boxBody 1 = Field (\x tx -> tx {_outputsX = x}) (D (decodeStrictSeq fromCBOR))
boxBody 4 = Field (\x tx -> tx {_certsX = x}) (D (decodeStrictSeq fromCBOR))
boxBody 5 = Field (\x tx -> tx {_wdrlsX = x}) From
boxBody 2 = Field (\x tx -> tx {_txfeeX = x}) From
boxBody 3 = Field (\x tx -> tx {_ttlX = x}) From
boxBody 6 = Field (\x tx -> tx {_txUpdateX = x}) (DD omitStrictNothingDual)
boxBody 7 = Field (\x tx -> tx {_mdHashX = x}) (DD omitStrictNothingDual)
boxBody n = Field (\_ t -> t) (Invalid n)

-- | Tells how to serialise each field, and what tag to label it with in the
--   serialisation. boxBody and txSparse should be Duals, visually inspect
--   The key order looks strange but was choosen for backward compatibility.
txSparse :: ProperTo era => TxBodyX era -> Encode ( 'Closed 'Sparse) (TxBodyX era)
txSparse (TxBodyX input output cert wdrl fee ttl update hash) =
  Keyed (\i o f t c w u h -> TxBodyX i o c w f t u h)
    !> Key 0 (E encodeFoldable input) -- We don't have to send these in TxBodyX order
    !> Key 1 (E encodeFoldable output) -- Just hack up a fake constructor with the lambda.
    !> Key 2 (To fee)
    !> Key 3 (To ttl)
    !> Omit null (Key 4 (E encodeFoldable cert))
    !> Omit (null . unWdrl) (Key 5 (To wdrl))
    !> Omit isSNothing (Key 6 (ED omitStrictNothingDual update))
    !> Omit isSNothing (Key 7 (ED omitStrictNothingDual hash))
-}