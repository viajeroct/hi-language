{-
In this file located just basic types.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}

module HW5.Base
  ( HiAction (..)
  , HiError (..)
  , HiExpr (..)
  , HiFun (..)
  , HiMonad (..)
  , HiValue (..)
  ) where

import Data.ByteString (ByteString)
import Data.Map        (Map)
import Data.Sequence   (Seq)
import Data.Text       (Text)
import Data.Time.Clock (UTCTime)

import GHC.Generics          (Generic)
import Codec.Serialise.Class (Serialise)

-- | Hi functions.
data HiFun
  = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Show, Eq, Ord, Generic, Serialise)

-- | Hi values.
data HiValue
  = HiValueFunction HiFun
  | HiValueBool     Bool
  | HiValueNumber   Rational
  | HiValueNull
  | HiValueString   Text
  | HiValueList     (Seq HiValue)
  | HiValueBytes    ByteString
  | HiValueAction   HiAction
  | HiValueTime     UTCTime
  | HiValueDict     (Map HiValue HiValue)
  deriving (Show, Eq, Ord, Generic, Serialise)

-- | Hi expressions.
data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun   HiExpr
  | HiExprDict  [(HiExpr, HiExpr)]
  deriving  (Show, Eq, Ord, Generic, Serialise)

-- | Hi errors.
data HiError
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show, Eq, Ord, Generic, Serialise)

-- | Hi actions.
data HiAction
  = HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Show, Eq, Ord, Generic, Serialise)

-- | Hi monad.
class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue
