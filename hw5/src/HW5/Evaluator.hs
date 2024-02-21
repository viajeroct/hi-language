{-
In this file located heart of my code - all what connected with evaluation.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}

module HW5.Evaluator
  ( eval
  ) where

import Codec.Compression.Zlib (bestCompression, compressLevel,
                               compressWith, decompress, defaultCompressParams)
import Codec.Serialise        (deserialise, serialise)
import Text.Read              (readMaybe)

import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable        (toList)
import Data.Maybe           (fromMaybe)
import Data.Sequence        (Seq (..), (><))
import Data.Text.Encoding   (decodeUtf8', encodeUtf8)
import Data.Time.Clock      (addUTCTime, diffUTCTime)
import Data.Word            (Word8)

import qualified Data.Sequence   as DSeq
import qualified Data.Text       as DText
import qualified Data.Map        as DMap
import qualified Data.ByteString as DByte

import HW5.Base
import HW5.Utility

import Control.Monad.Except

-- | Evaluates hi expression.
eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . evalExpr

-- | Entry point, evals expression.
evalExpr :: HiExpr -> EvalRes m
evalExpr = \case
  HiExprValue x         -> return x
  HiExprRun   x         -> evalIO x
  HiExprApply func args -> evalApply func args
  HiExprDict  x         -> HiValueDict . DMap.fromList <$> mapM map' x
  where
    map' (a, b) = (,) <$> evalExpr a <*> evalExpr b

-- | Evals IO.
evalIO :: HiExpr -> EvalRes m
evalIO param = evalExpr param >>= \case
  HiValueAction action -> lift (runAction action)
  _                    -> invalid

-- | Evals application.
evalApply :: HiExpr -> [HiExpr] -> EvalRes m
evalApply func args = evalExpr func >>= \case
  HiValueFunction fun -> function fun args
  s@(HiValueString _) -> slice s args
  s@(HiValueList   _) -> slice s args
  s@(HiValueBytes  _) -> slice s args
  s@(HiValueDict   _) -> slice s args
  _                   -> throwError HiErrorInvalidFunction

-- | Evaluates function.
function :: HiFun -> [HiExpr] -> EvalRes m
function = \case
  HiFunAdd            -> binary evalAdd
  HiFunSub            -> binary evalSub
  HiFunMul            -> binary evalMul
  HiFunDiv            -> binary evalDiv
  HiFunNot            -> unary mkNot
  HiFunAnd            -> lazyBinary lazyAnd
  HiFunOr             -> lazyBinary lazyOr
  HiFunLessThan       -> binary (/</)
  HiFunGreaterThan    -> binary (/>/)
  HiFunEquals         -> binary (/==/)
  HiFunNotLessThan    -> binary (/>=/)
  HiFunNotGreaterThan -> binary (/<=/)
  HiFunNotEquals      -> binary (//=/)
  HiFunIf             -> ternary
  HiFunLength         -> unary calcLen
  HiFunReverse        -> unary reverse'
  HiFunToUpper        -> unary $ wrapText DText.toUpper
  HiFunToLower        -> unary $ wrapText DText.toLower
  HiFunTrim           -> unary $ wrapText DText.strip
  HiFunList           -> fmap (HiValueList . DSeq.fromList) . mapM evalExpr
  HiFunRange          -> binary range
  HiFunFold           -> binary makeFold
  HiFunPackBytes      -> unary packValue
  HiFunUnpackBytes    -> unary unpack
  HiFunEncodeUtf8     -> unary evalEncode
  HiFunDecodeUtf8     -> unary evalDecode
  HiFunZip            -> unary evalCompress
  HiFunUnzip          -> unary evalDecompress
  HiFunSerialise      -> unary $ return . HiValueBytes . toStrict . serialise
  HiFunDeserialise    -> unary deserialise'
  HiFunRead           -> unary actionRead
  HiFunWrite          -> binary actionWrite
  HiFunMkDir          -> unary actionMkDir
  HiFunChDir          -> unary actionChDir
  HiFunParseTime      -> unary getTime
  HiFunRand           -> binary randValue
  HiFunEcho           -> unary makeEcho
  HiFunCount          -> unary count
  HiFunKeys           -> unary dictKeys
  HiFunValues         -> unary dictValues
  HiFunInvert         -> unary evalInvert

-- | Evaluates length.
calcLen :: Unary m
calcLen (HiValueString x) = getLen' <~> x
calcLen (HiValueList   x) = getLen' <~> x
calcLen _                 = invalid

-- | Wraps text.
wrapText :: (DText.Text -> DText.Text) -> HiValue -> EvalRes m
wrapText f (HiValueString text) = HiValueString <~> f text
wrapText _ _                    = invalid

-- | Abstract compare function.
cmp :: (HiValue -> HiValue -> Bool) -> Binary m
cmp f x y = HiValueBool <~> f x y

-- | Compare operations.
(/</)  :: Binary m
(/>/)  :: Binary m
(/==/) :: Binary m
(/>=/) :: Binary m
(/<=/) :: Binary m
(//=/) :: Binary m

(/</)  =  cmp (<)
(/>/)  =  cmp (>)
(/==/) =  cmp (==)
(/>=/) =  cmp (>=)
(/<=/) =  cmp (<=)
(//=/) =  cmp (/=)

-- | Unary functions.
unary :: Unary m -> [HiExpr] -> EvalRes m
unary f [x] = evalExpr x >>= f
unary _ _   = throwError HiErrorArityMismatch

-- | Binary functions.
binary :: Binary m -> [HiExpr] -> EvalRes m
binary f [x, y] = join $ f <$> evalExpr x <*> evalExpr y
binary _ _      = throwError HiErrorArityMismatch

-- | Lazy binary functions.
lazyBinary :: (HiValue -> HiExpr -> EvalRes m) -> [HiExpr] -> EvalRes m
lazyBinary f [x, y] = evalExpr x >>= flip f y
lazyBinary _ _      = throwError HiErrorArityMismatch

-- | Gets values of dict.
dictValues :: Unary m
dictValues (HiValueDict x) = HiValueList . DSeq.fromList . DMap.elems <~> x
dictValues _               = invalid

-- | Gets keys of dict.
dictKeys :: Unary m
dictKeys (HiValueDict x) = HiValueList . DSeq.fromList . DMap.keys <~> x
dictKeys _               = invalid

-- | Inverts
evalInvert :: Unary m
evalInvert (HiValueDict x) = HiValueDict . DMap.map HiValueList . DMap.fromListWith (><) . map (\(key, val) -> (val, DSeq.singleton key))
                             <~> DMap.toList x
evalInvert _               = invalid

-- | Evaluates count function.
count :: Unary m
count (HiValueString x) = calc (DText.unpack x) (HiValueString . DText.singleton)
count (HiValueBytes  x) = calc (map fromIntegral (DByte.unpack x)) HiValueNumber
count (HiValueList   x) = calc (toList x) id
count _                 = invalid

-- | Abstract counter.
calc :: Ord a => [a] -> (a -> HiValue) -> EvalRes m
calc x f = HiValueDict . DMap.mapKeys f . DMap.map HiValueNumber . DMap.fromListWith (+) <~> zip x (replicate (length x) 1)

-- | Echoes value.
makeEcho :: Unary m
makeEcho (HiValueString x) = HiValueAction . HiActionEcho <~> x
makeEcho _                 = invalid

-- | Gets random value.
randValue :: Binary m
randValue (HiValueNumber f) (HiValueNumber t) = fmap HiValueAction (HiActionRand <$> toInt f <*> toInt t)
randValue _ _                                 = invalid

-- | Gets time.
getTime :: Unary m
getTime (HiValueString x) = maybe HiValueNull HiValueTime . readMaybe . DText.unpack <~> x
getTime _                 = invalid

-- | Deserialise impl.
deserialise' :: Unary m
deserialise' (HiValueBytes x) = deserialise . fromStrict <~> x
deserialise' _                = invalid

-- | Compress operation.
evalCompress :: Unary m
evalCompress (HiValueBytes x) = HiValueBytes . toStrict . compressWith defaultCompressParams { compressLevel = bestCompression }
                                <~> fromStrict x
evalCompress _ = invalid

-- | Decompress operation.
evalDecompress :: Unary m
evalDecompress (HiValueBytes x) = HiValueBytes . toStrict . decompress . fromStrict <~> x
evalDecompress _                = invalid

-- | Decode operation.
evalDecode :: Unary m
evalDecode (HiValueBytes x) = return $ case decodeUtf8' x of
  Left _  -> HiValueNull
  Right y -> HiValueString y
evalDecode _ = invalid

-- | Encode operation.
evalEncode :: Unary m
evalEncode (HiValueString x) = HiValueBytes . encodeUtf8 <~> x
evalEncode _                 = invalid

-- | Pack operation.
packValue :: Unary m
packValue (HiValueList x) = HiValueBytes . DByte.pack . toList <$> mapM valueToByte x
packValue _               = invalid

-- | Unpack function.
unpack :: Unary m
unpack (HiValueBytes x) = HiValueList . DSeq.fromList <$> mapM (return . HiValueNumber . fromIntegral) (DByte.unpack x)
unpack _                = invalid

-- | Converts HiValue to Word8
valueToByte :: HiMonad m => HiValue -> ExceptT HiError m Word8
valueToByte (HiValueNumber number) = convertToInt number >>= \i -> if isByte i
  then fromIntegral <~> i
  else throwError HiErrorInvalidArgument
valueToByte _ = throwError HiErrorInvalidArgument

-- | Checks that number is byte.
isByte :: (Ord a, Num a) => a -> Bool
isByte x = x >= 0 && x <= 255

-- | Reverse operation.
reverse' :: Unary m
reverse' (HiValueString x) = HiValueString . rev <~> x
reverse' (HiValueList   x) = HiValueList   . rev <~> x
reverse' _                 = invalid

-- | Makes fold operation.
makeFold :: Binary m
makeFold (HiValueFunction func) (HiValueList (x :<| xs)) = case func of
  HiFunAnd            -> foldM lazyAnd x $ customMap xs DSeq.Empty
  HiFunOr             -> foldM lazyOr x  $ customMap xs DSeq.Empty
  HiFunAdd            -> foldM evalAdd x xs
  HiFunSub            -> foldM evalSub x xs
  HiFunMul            -> foldM evalMul x xs
  HiFunDiv            -> foldM evalDiv x xs
  HiFunLessThan       -> foldM (/</) x xs
  HiFunGreaterThan    -> foldM (/>/) x xs
  HiFunEquals         -> foldM (/==/) x xs
  HiFunNotLessThan    -> foldM (/>=/) x xs
  HiFunNotGreaterThan -> foldM (/<=/) x xs
  HiFunNotEquals      -> foldM (//=/) x xs
  HiFunRange          -> foldM randValue x xs
  HiFunFold           -> foldM makeFold x xs
  HiFunWrite          -> foldM actionWrite x xs
  HiFunRand           -> foldM randValue x xs
  _                   -> invalid
makeFold _ (HiValueList DSeq.Empty) = return HiValueNull
makeFold _ _                        = invalid

-- | If evaluation.
ternary :: [HiExpr] -> EvalRes m
ternary [cond, tr, fl] = evalExpr cond >>= \case
  HiValueBool r -> evalExpr $ if r then tr else fl
  _             -> invalid
ternary _ = throwError HiErrorArityMismatch

-- | Lazy and.
lazyAnd :: HiValue -> HiExpr -> EvalRes m
lazyAnd x y
  | lazy x     = return x
  | otherwise  = evalExpr y

-- | Lazy or.
lazyOr :: HiValue -> HiExpr -> EvalRes m
lazyOr x y
  | lazy x     = evalExpr y
  | otherwise  = return x

-- | Not function.
mkNot :: Unary m
mkNot (HiValueBool x) = HiValueBool <~> not x
mkNot _               = invalid

-- | Lazy helper for or and and.
lazy :: HiValue -> Bool
lazy = \case
  HiValueNull       -> True
  HiValueBool False -> True
  _                 -> False

-- | Custom map for Seq.
customMap :: Seq HiValue -> Seq HiExpr -> Seq HiExpr
customMap DSeq.Empty res = res
customMap (x :<| xs) res = customMap xs (HiExprValue x :<| res)

-- | Makes range.
range :: Binary m
range (HiValueNumber l) (HiValueNumber r) = HiValueList . DSeq.fromList . map HiValueNumber <~> [l..r]
range _ _                                 = invalid

-- | Makes slice.
slice :: HiValue -> [HiExpr] -> EvalRes m
slice (HiValueString x) l@[_]     = unary  (byIndex' x) l
slice (HiValueList   x) l@[_]     = unary  (byIndex' x) l
slice (HiValueBytes  x) l@[_]     = unary  (byIndex' x) l
slice (HiValueDict   x) l@[_]     = unary  (find     x) l
slice (HiValueString x) ll@[_, _] = binary (slice'   x) ll
slice (HiValueList   x) ll@[_, _] = binary (slice'   x) ll
slice (HiValueBytes  x) ll@[_, _] = binary (slice'   x) ll
slice _ _                         = throwError HiErrorArityMismatch

-- | Finds value in dict.
find :: DMap.Map HiValue HiValue -> HiValue -> EvalRes m
find d k = fromMaybe HiValueNull <~> DMap.lookup k d

-- | Slice helper.
slice' :: Container a => a -> HiValue -> HiValue -> EvalRes m
slice' dt (HiValueNumber f) (HiValueNumber t) = callSlice dt <$> shiftIndex dt f <*> shiftIndex dt t
slice' dt HiValueNull     r@(HiValueNumber _) = slice' dt (HiValueNumber 0) r
slice' dt l@(HiValueNumber _)    HiValueNull  = slice' dt l $ getLen' dt
slice' dt HiValueNull            HiValueNull  = slice' dt (HiValueNumber 0) $ getLen' dt
slice' _ _ _                                  = invalid

-- | Subs abstract values.
evalSub :: Binary m
evalSub (HiValueNumber x) (HiValueNumber y) = HiValueNumber <~> x - y
evalSub (HiValueTime   x) (HiValueTime   y) = HiValueNumber . realToFrac . diffUTCTime x <~> y
evalSub _ _                                 = invalid

-- | Evaluates div operation.
evalDiv :: Binary m
evalDiv (HiValueString x) (HiValueString y) = HiValueString <~> removeSlash x <> DText.pack "/" <> removeSlash y
evalDiv (HiValueNumber x) (HiValueNumber y)
  | y == 0    = throwError HiErrorDivideByZero
  | otherwise = HiValueNumber <~> x / y
evalDiv _ _ = invalid

-- | Function for removing slashes.
removeSlash :: DText.Text -> DText.Text
removeSlash = DText.dropWhileEnd (== '/')

-- | Adds abstract values.
evalAdd :: Binary m
evalAdd (HiValueString x) (HiValueString y) = HiValueString <~> x <> y
evalAdd (HiValueList   x) (HiValueList   y) = HiValueList   <~> x <> y
evalAdd (HiValueBytes  x) (HiValueBytes  y) = HiValueBytes  <~> x <> y
evalAdd (HiValueTime   x) (HiValueNumber y) = HiValueTime   <~> addUTCTime (realToFrac y) x
evalAdd (HiValueNumber x) (HiValueNumber y) = HiValueNumber <~> x + y
evalAdd _ _                                 = invalid

-- | Muls abstract values.
evalMul :: Binary m
evalMul (HiValueNumber x) (HiValueString y) = stimes' x y HiValueString
evalMul (HiValueNumber x) (HiValueList   y) = stimes' x y HiValueList
evalMul (HiValueNumber x) (HiValueBytes  y) = stimes' x y HiValueBytes
evalMul (HiValueNumber x) (HiValueNumber y) = HiValueNumber <~> x * y
evalMul x y
  | x > y     = evalMul y x
  | otherwise = invalid

-- | Cd dir.
actionChDir :: Unary m
actionChDir (HiValueString x) = HiValueAction . HiActionChDir . DText.unpack <~> x
actionChDir _                 = invalid

-- | Makes dir.
actionMkDir :: Unary m
actionMkDir (HiValueString x) = HiValueAction . HiActionMkDir . DText.unpack <~> x
actionMkDir _                 = invalid

-- | Read action.
actionRead :: Unary m
actionRead (HiValueString x) = HiValueAction . HiActionRead . DText.unpack <~> x
actionRead _                 = invalid

-- | Write action.
actionWrite :: Binary m
actionWrite (HiValueString x) (HiValueString y) = HiValueAction . HiActionWrite (DText.unpack x) <~> encodeUtf8 y
actionWrite (HiValueString x) (HiValueBytes  y) = HiValueAction . HiActionWrite (DText.unpack x) <~> y
actionWrite (HiValueString x)                y  = HiValueAction . HiActionWrite (DText.unpack x) <~> toStrict (serialise y)
actionWrite _ _ = invalid
