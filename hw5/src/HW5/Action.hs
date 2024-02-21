{-
In this file implemented all logic
for IO functions.
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}

module HW5.Action
  ( HIO (..)
  , HiPermission (..)
  , PermissionException (..)
  ) where

import Control.Exception.Base (Exception, throwIO)

import Data.Set               (Set, member)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import Data.Text.Encoding     (decodeUtf8')
import Data.Functor           (($>))

import System.Directory       (doesFileExist, listDirectory, createDirectory,
                               setCurrentDirectory, getCurrentDirectory)
import System.Random.Stateful (randomRIO)

import qualified Data.ByteString.Lazy.Char8 as DByte8
import qualified Data.Sequence              as DSeq
import qualified Data.Text                  as DText

import HW5.Base
import HW5.Utility

-- | Possible permissions.
data HiPermission = AllowRead | AllowWrite | AllowTime
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Permission exception. If action runs without permission.
newtype PermissionException = PermissionRequired HiPermission
  deriving Show

-- | Just Exception for PermissionException.
instance Exception PermissionException

-- | Custom hi IO.
newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
  deriving Functor

-- | Applicative instance for HIO.
instance Applicative HIO where
  pure a  = HIO $ \_ -> pure a
  x <*> y = HIO $ \perm -> runHIO x perm <*> runHIO y perm

-- | Monad instance for HIO.
instance Monad HIO where
  x >>= f = HIO $ \perm -> runHIO x perm >>= \a -> runHIO (f a) perm

-- | HiMonad instance for HIO.
instance HiMonad HIO where
  runAction = \case
    HiActionRead  x      -> check AllowRead  $ doesFileExist x >>= readOrList x
    HiActionWrite x dt   -> check AllowWrite $ DByte8.writeFile x (DByte8.fromStrict dt) $> HiValueNull
    HiActionMkDir x      -> check AllowWrite $ createDirectory x                         $> HiValueNull
    HiActionChDir x      -> check AllowRead  $ setCurrentDirectory x                     $> HiValueNull
    HiActionCwd          -> check AllowRead  $ HiValueString . DText.pack               <$> getCurrentDirectory
    HiActionNow          -> check AllowTime  $ HiValueTime . systemToUTCTime            <$> getSystemTime
    HiActionEcho text    -> check AllowWrite $ putStrLn (DText.unpack text)              $> HiValueNull
    HiActionRand from to -> HIO $ \_ -> HiValueNumber . fromIntegral                    <$> randomRIO (from, to)

-- | Reads data from file or lists files in directory.
readOrList :: FilePath -> Bool -> IO HiValue
readOrList path flag = if flag then readData path else listDir path

-- | Lists all files in directory.
listDir :: FilePath -> IO HiValue
listDir path = HiValueList . DSeq.fromList . fmap (HiValueString . DText.pack) <$> listDirectory path

-- | Reads data from file.
readData :: FilePath -> IO HiValue
readData path = do
  bytes <- DByte8.readFile path
  let decoded = decodeUtf8' (DByte8.toStrict bytes)
  case decoded of
    Left _     -> HiValueBytes  <~> DByte8.toStrict bytes
    Right text -> HiValueString <~> text

-- | Checks that permission is granted. Fails or runs function with granted permission.
check :: HiPermission -> IO HiValue -> HIO HiValue
check p f = HIO $ \ps -> getByPermission (member p ps) f p
  where
    getByPermission cond f' p'
      | cond      = f'
      | otherwise = throwIO $ PermissionRequired p'
