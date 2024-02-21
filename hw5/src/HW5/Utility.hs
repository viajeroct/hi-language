{-
In this file implemented utility functions.
Some of them just for convenience, some of them
are usefull for Evaluator.hs
-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}

module HW5.Utility
  ( Container (rev)
  , getToken
  , callSlice
  , shiftIndex
  , toInt
  , convertToInt
  , getLen'
  , byIndex'
  , (<~>)
  , EvalRes
  , Unary
  , Binary
  , invalid
  , stimes'
  ) where

import qualified Data.Sequence   as DSeq
import qualified Data.Text       as DText
import qualified Data.ByteString as DByte

import Data.Semigroup  (stimes)
import Data.Ratio      (denominator, numerator)

import Control.Monad.Except

import HW5.Base

{-
About function bellow.
I think it's just more beautiful to write f <~> x.
return $ f x - it's not so beautiful.

Also in some places it's possible to use do-notation instead of >>=.
But I think >>= is more beautiful and shorter.
-}

-- | Function for beautiful code style.
infixl 1 <~>
(<~>) :: Monad m => (t -> a) -> t -> m a
(<~>) f = return . ($) f

-- | Some types for beautiful code style.
type EvalRes m = HiMonad m => ExceptT HiError m HiValue
type Unary   m = HiValue -> EvalRes m
type Binary  m = HiValue -> Unary m

-- | Just for shortness.
invalid :: EvalRes m
invalid = throwError HiErrorInvalidArgument



-- | Maps function to it name.
getToken :: HiFun -> String
getToken = \case
  HiFunMul            -> "mul"
  HiFunDiv            -> "div"
  HiFunAdd            -> "add"
  HiFunSub            -> "sub"
  HiFunAnd            -> "and"
  HiFunOr             -> "or"
  HiFunLessThan       -> "less-than"
  HiFunGreaterThan    -> "greater-than"
  HiFunEquals         -> "equals"
  HiFunNotLessThan    -> "not-less-than"
  HiFunNotGreaterThan -> "not-greater-than"
  HiFunNotEquals      -> "not-equals"
  HiFunNot            -> "not"
  HiFunIf             -> "if"
  HiFunLength         -> "length"
  HiFunToUpper        -> "to-upper"
  HiFunToLower        -> "to-lower"
  HiFunReverse        -> "reverse"
  HiFunTrim           -> "trim"
  HiFunList           -> "list"
  HiFunRange          -> "range"
  HiFunFold           -> "fold"
  HiFunPackBytes      -> "pack-bytes"
  HiFunUnpackBytes    -> "unpack-bytes"
  HiFunEncodeUtf8     -> "encode-utf8"
  HiFunDecodeUtf8     -> "decode-utf8"
  HiFunZip            -> "zip"
  HiFunUnzip          -> "unzip"
  HiFunSerialise      -> "serialise"
  HiFunDeserialise    -> "deserialise"
  HiFunRead           -> "read"
  HiFunWrite          -> "write"
  HiFunMkDir          -> "mkdir"
  HiFunChDir          -> "cd"
  HiFunParseTime      -> "parse-time"
  HiFunRand           -> "rand"
  HiFunEcho           -> "echo"
  HiFunCount          -> "count"
  HiFunKeys           -> "keys"
  HiFunValues         -> "values"
  HiFunInvert         -> "invert"



-- | Abstract container (supports list operations).
class (Monoid container) => Container container where
  -- | Gets value by index.
  byIndex :: container -> Int -> HiValue

  -- | Gets slice.
  getSlice :: container -> Int -> Int -> HiValue

  -- | Wraps given container into corresponding HiValue.
  wrap :: container -> (container -> HiValue)

  -- | Returns length of container.
  getLen :: container -> Int

  -- | Reverses container.
  rev :: container -> container

-- | Container instance for ByteString.
instance Container DByte.ByteString where
  byIndex cont i    = checkIndex cont i (HiValueNumber . fromIntegral . DByte.index cont)
  getSlice cont i j = HiValueBytes . DByte.drop i . DByte.take j $ cont
  wrap _            = HiValueBytes
  getLen            = DByte.length
  rev               = DByte.reverse

-- | Container instance for Seq.
instance Container (DSeq.Seq HiValue) where
  byIndex cont i    = checkIndex cont i (DSeq.index cont)
  getSlice cont i j = HiValueList . DSeq.drop i . DSeq.take j $ cont
  wrap _            = HiValueList
  getLen            = DSeq.length
  rev               = DSeq.reverse

-- | Container instance for Text.
instance Container DText.Text where
  byIndex cont i    = checkIndex cont i (HiValueString . DText.singleton . DText.index cont)
  getSlice cont i j = HiValueString . DText.drop i . DText.take j $ cont
  wrap _            = HiValueString
  getLen            = DText.length
  rev               = DText.reverse



-- | Gets index.
byIndex' :: Container container => container -> HiValue -> EvalRes m
byIndex' dt (HiValueNumber index) = byIndex dt <$> toInt index
byIndex' _ _                      = invalid

-- | Function, that checks, that index is in [0, size].
checkIndex :: Container container => container -> Int -> (Int -> HiValue) -> HiValue
checkIndex dt index f
  | index >= 0 && index < size = f index
  | otherwise                  = HiValueNull
  where size = getLen dt

-- | Calls getSlice function, checking that i < j.
callSlice :: Container container => container -> Int -> Int -> HiValue
callSlice dt from to
  | from < to = getSlice dt from to
  | otherwise = wrap dt mempty

-- | Shifts index to interval [0, size].
shiftIndex :: (HiMonad m, Container container) => container -> Rational -> ExceptT HiError m Int
shiftIndex dt index = toInt index >>= \ind ->
  cut size' <~> inc ind
  where
    size' = getLen dt
    cut size pos
      | pos < 0    = 0
      | pos > size = size
      | otherwise  = pos
    inc ind'
      | ind' < 0  = inc $ ind' + size'
      | otherwise = ind'

-- | Calls getLen function, wrapping it in HiValueNumber.
getLen' :: Container container => container -> HiValue
getLen' = HiValueNumber . fromIntegral . getLen

-- | Safely converts rational number to Integer.
convertToInt :: HiMonad m => Rational -> ExceptT HiError m Integer
convertToInt rational
  | denominator rational == 1 = numerator <~> rational
  | otherwise                 = throwError HiErrorInvalidArgument

-- | Safely converts Integer number to Int.
toInt :: HiMonad m => Rational -> ExceptT HiError m Int
toInt x = fmap fromIntegral (convertToInt x)

-- | Stimes function wrapper.
stimes' :: Semigroup a => Rational -> a -> (a -> HiValue) -> EvalRes m
stimes' n dt op = convertToInt n >>= \res -> if res > 0 then op <~> stimes res dt else invalid
