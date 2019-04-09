{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Slotting.Properties
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Data.Data (Constr, toConstr)
import Formatting (build, sformat)

import Hedgehog
  ( Group
  , property
  , forAll
  , (===)
  , success
  , checkSequential
  )
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Property (failWith)
import qualified Hedgehog.Range as Range

import Cardano.Chain.Slotting
  ( addSlotNumber
  , FlatSlotId(..)
  , EpochSlots(..)
  , LocalSlotIndex(..)
  , LocalSlotIndexError(..)
  , flattenSlotId
  , unflattenSlotId
  , localSlotIndexSucc
  , localSlotIndexPred
  , localSlotIndexToEnum
  , localSlotIndexFromEnum
  , subSlotNumber
  )
import Test.Cardano.Chain.Slotting.Gen
  ( genFlatSlotId
  , genLocalSlotIndex
  , genLsiEpochSlots
  , genSlotId
  , genConsistentSlotIdEpochSlots
  )
import Test.Options (TestScenario, TSProperty, withTestsTS)

-- NB: `genLsiEpochSlots` is used because `LocalSlotIndex` is restricted to
-- `Word16` and therefore the highest `EpochSlots` you can have currently
-- is `maxBound :: Word16`.

--------------------------------------------------------------------------------
-- LocalSlotIndex
--------------------------------------------------------------------------------

-- Check that `genLocalSlotIndex` does not `panic` for
-- different values of `EpochSlots`. NB: we use `genLsiEpochSlots`
-- to generate values of `EpochSlots` restricted to a maximum boundary
-- of `Word16` because `LocalSlotIndex` is restricted to `Word16` values.
ts_prop_genLocalSlotIndex :: TSProperty
ts_prop_genLocalSlotIndex = withTestsTS 100 . property $ do
  sc  <- forAll genLsiEpochSlots
  lsi <- forAll $ genLocalSlotIndex sc
  case lsi of
    UnsafeLocalSlotIndex _ -> success

-- Check that `localSlotIndexToEnum` fails for
-- `LocalSlotIndex` values that exceed `EpochSlots`
ts_prop_localSlotIndexToEnumOverflow :: TSProperty
ts_prop_localSlotIndexToEnumOverflow = withTestsTS 100 . property $ do
  sc <- forAll genLsiEpochSlots
  let lsi = 1 + unEpochSlots sc
  assertIsLeftConstr
    dummyLocSlotIndEnumOverflow
    (localSlotIndexToEnum sc (fromIntegral lsi))

-- Check that `localSlotIndexToEnum` fails for
-- `LocalSlotIndex` values that are negative.
ts_prop_localSlotIndexToEnumUnderflow :: TSProperty
ts_prop_localSlotIndexToEnumUnderflow = withTestsTS 100 . property $ do
  tVal <- forAll (Gen.int (Range.constant (negate 1) minBound))
  sc   <- forAll genLsiEpochSlots
  assertIsLeftConstr dummyLocSlotIndEnumUnderflow (localSlotIndexToEnum sc tVal)

-- Check that `localSlotIndexPred` does not fail
-- for allowed values of `LocalSlotIndex` and `EpochSlots`.
ts_prop_localSlotIndexPred :: TSProperty
ts_prop_localSlotIndexPred =
  withTestsTS 100
    . property
    $ do
        es  <- forAll $ Gen.filter (\x -> unEpochSlots x /= 1) genLsiEpochSlots
        -- Filter out LocalSlotIndex = 0 and EpochSlots = 1
        -- because you can't find the predecessor of the 0th slot.
        lsi <- forAll
          $ Gen.filter (/= UnsafeLocalSlotIndex 0) (genLocalSlotIndex es)
        assertIsRight $ localSlotIndexPred es lsi

-- Check that `localSlotIndexPred` fails for
-- the lower boundary of `LocalSlotIndex`. In
-- other words, the 0th slot does not have
-- a predecessor.
ts_prop_localSlotIndexPredMinbound :: TSProperty
ts_prop_localSlotIndexPredMinbound = withTestsTS 100 . property $ do
  eSlots <- forAll genLsiEpochSlots
  assertIsLeftConstr
    dummyLocSlotIndEnumUnderflow
    (localSlotIndexPred eSlots (UnsafeLocalSlotIndex 0))

-- Check that `localSlotIndexSucc` does not fail
-- for allowed values of `LocalSlotIndex` and `EpochSlots`.
ts_prop_localSlotIndexSucc :: TSProperty
ts_prop_localSlotIndexSucc =
  withTestsTS 100
    . property
    $ do
        es  <- forAll genLsiEpochSlots
        -- Generate a `LocalSlotIndex` at least two less than the `EpochSlots`
        -- to avoid overflow errors as `LocalSlotIndex` starts
        -- from 0th slot.
        lsi <- forAll $ genLocalSlotIndex es
        let esPlus2 = EpochSlots $ unEpochSlots es + 2
        assertIsRight $ localSlotIndexSucc esPlus2 lsi

-- Check that `localSlotIndexSucc` fails for
-- the upper boundary of `LocalSlotIndex`. In
-- other words, the final slot does not have
-- a successor (in terms of `LocalSlotIndex`,
-- this would actually mean moving to the next epoch).
ts_prop_localSlotIndexSuccMaxbound :: TSProperty
ts_prop_localSlotIndexSuccMaxbound = withTestsTS 100 . property $ do
  sc <- forAll genLsiEpochSlots
  assertIsLeftConstr
    dummyLocSlotIndEnumOverflow
    ( localSlotIndexSucc sc
    $ UnsafeLocalSlotIndex
    $ 1
    + (fromIntegral $ unEpochSlots sc)
    )

-- Check that `localSlotIndexSucc . localSlotIndexPred == id`.
ts_prop_localSlotIndexSuccPredisId :: TSProperty
ts_prop_localSlotIndexSuccPredisId = withTestsTS 100 . property $ do
  sc  <- forAll genLsiEpochSlots
  lsi <- forAll
    $ Gen.filter (\x -> unLocalSlotIndex x /= 0) (genLocalSlotIndex sc)
  let predSucc = localSlotIndexPred sc lsi >>= localSlotIndexSucc sc
  compareValueRight lsi predSucc

-- Check that `localSlotIndexPred . localSlotIndexSucc == id`.
ts_prop_localSlotIndexPredSuccisId :: TSProperty
ts_prop_localSlotIndexPredSuccisId = withTestsTS 100 . property $ do
  es  <- forAll genLsiEpochSlots
  lsi <- forAll $ genLocalSlotIndex es
  let
    esPlus2  = EpochSlots $ unEpochSlots es + 2
    succPred = localSlotIndexSucc esPlus2 lsi >>= localSlotIndexPred esPlus2
  compareValueRight lsi succPred

-- Check that `localSlotIndexToEnum . localSlotIndexFromEnum == id`.
ts_prop_localSlotIndexToEnumFromEnum :: TSProperty
ts_prop_localSlotIndexToEnumFromEnum = withTestsTS 100 . property $ do
  sc   <- forAll genLsiEpochSlots
  iLsi <- forAll $ genLocalSlotIndex sc
  let fLsi = localSlotIndexToEnum sc $ localSlotIndexFromEnum iLsi
  compareValueRight iLsi fLsi

-- Check that `localSlotIndexFromEnum . localSlotIndexToEnum == id`.
ts_prop_localSlotIndexFromEnumToEnum :: TSProperty
ts_prop_localSlotIndexFromEnumToEnum = withTestsTS 100 . property $ do
  sc <- forAll genLsiEpochSlots
  let sIndex = fromIntegral $ unEpochSlots sc - 1 :: Int
  let lsi    = localSlotIndexToEnum sc sIndex
  case lsi of
    Left err ->
      withFrozenCallStack $ failWith Nothing (show $ sformat build err)
    Right lsi' -> localSlotIndexFromEnum lsi' === sIndex

--------------------------------------------------------------------------------
-- Dummy values for constructor comparison in assertIsLeftConstr tests
--------------------------------------------------------------------------------

dummyLocSlotIndEnumOverflow :: Constr
dummyLocSlotIndEnumOverflow =
  toConstr $ LocalSlotIndexEnumOverflow (EpochSlots 1) 1

dummyLocSlotIndEnumUnderflow :: Constr
dummyLocSlotIndEnumUnderflow = toConstr $ LocalSlotIndexEnumUnderflow 1

dummyLocSlotIndIndexOverflow :: Constr
dummyLocSlotIndIndexOverflow =
  toConstr $ LocalSlotIndexOverflow (EpochSlots 1) 1

--------------------------------------------------------------------------------
-- SlotId
--------------------------------------------------------------------------------

-- Check that `unflattenSlotId` does not panic for
-- allowed values of `EpochSlots` and `FlatSlotId`.
ts_prop_unflattenSlotId :: TSProperty
ts_prop_unflattenSlotId = withTestsTS 100 . property $ do
  sc   <- forAll genLsiEpochSlots
  fsId <- forAll $ genFlatSlotId
  _    <- pure $ unflattenSlotId sc fsId
  success

-- Check that `unflattenSlotId . flattenSlotId == id`.
ts_prop_unflattenFlattenSlotId :: TSProperty
ts_prop_unflattenFlattenSlotId = withTestsTS 100 . property $ do
  (sId, sc) <- forAll genConsistentSlotIdEpochSlots
  sId === unflattenSlotId sc (flattenSlotId sc sId)

-- Check that `genSlotId` does not panic for
-- allowed values of `EpochSlots`.
ts_prop_genSlotId :: TSProperty
ts_prop_genSlotId = withTestsTS 100 . property $ do
  sc <- forAll genLsiEpochSlots
  _  <- forAll $ genSlotId sc
  success

-- Check that `flattenSlotId . unflattenSlotId == id`.
ts_prop_flattenUnflattenSlotId :: TSProperty
ts_prop_flattenUnflattenSlotId = withTestsTS 100 . property $ do
  sc   <- forAll genLsiEpochSlots
  fsId <- forAll genFlatSlotId
  let unflatFlat = flattenSlotId sc $ unflattenSlotId sc fsId
  fsId === unflatFlat

-- Check that `addSlotNumber` actually adds.
ts_prop_addSlotNumber :: TSProperty
ts_prop_addSlotNumber = withTestsTS 100 . property $ do
  sc <- forAll genLsiEpochSlots
  fs <- forAll genFlatSlotId
  let added = fs + (FlatSlotId $ unEpochSlots sc)
  addSlotNumber sc fs === added

-- Check that `subSlotNumber` actually subtracts.
ts_prop_subSlotNumber :: TSProperty
ts_prop_subSlotNumber = withTestsTS 100 . property $ do
  sc <- forAll genLsiEpochSlots
  fs <- forAll genFlatSlotId
  let subtracted = fs - (FlatSlotId $ unEpochSlots sc)
  (subSlotNumber sc fs) === subtracted

tests :: TestScenario -> IO Bool
tests ts = checkSequential (($$discoverPropArg :: TestScenario -> Group) ts)
