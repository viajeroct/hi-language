{-
In this file implemented all connected with pretty.
-}

{-# LANGUAGE LambdaCase #-}

module HW5.Pretty
  ( prettyValue
  ) where

import Data.ByteString (ByteString, unpack)
import Data.Foldable   (toList)
import Data.Scientific (floatingOrInteger, fromRationalRepetendUnlimited)

import GHC.Real (Ratio ((:%)))
import Numeric  (showFFloat, showHex)

import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle)

import qualified Data.Map as DMap

import HW5.Utility
import HW5.Base

-- | Just alias.
type Res = Doc AnsiStyle

-- | Returns pretty value.
prettyValue :: HiValue -> Res
prettyValue = \case
  HiValueNumber   x -> prettyNumber x
  HiValueBool     x -> pretty $ if x then "true" else "false"
  HiValueFunction x -> pretty $ getToken x
  HiValueString   x -> viaShow x
  HiValueList     x -> pretty' lbracket ", " prettyValue (toList x) rbracket
  HiValueBytes    x -> prettyBytes x
  HiValueAction   x -> prettyAction x
  HiValueTime     x -> prettyUnary "parse-time" ("\"" <> show x <> "\"")
  HiValueDict     x -> pretty' lbrace ", " (\(k,v) -> prettyValue k <> colon <+> prettyValue v) (DMap.toList x) rbrace
  HiValueNull       -> pretty "null"

-- | Returns pretty actions.
prettyAction :: HiAction -> Res
prettyAction = \case
  HiActionRead  x       -> prettyUnary "read" $ show x
  HiActionWrite p b     -> prettyBinary "write" (show p) (show $ prettyBytes b)
  HiActionMkDir x       -> prettyUnary "mkdir" $ show x
  HiActionChDir x       -> prettyUnary "cd" $ show x
  HiActionRand  from to -> prettyBinary "rand" from to
  HiActionEcho  x       -> prettyUnary "echo" $ show x
  HiActionCwd           -> pretty "cwd"
  HiActionNow           -> pretty "now"

-- | Returns pretty unary.
prettyUnary :: String -> String -> Res
prettyUnary name arg = pretty name <> parens (pretty arg)

-- | Returns pretty binary.
prettyBinary :: (Pretty t, Pretty f) => String -> t -> f -> Res
prettyBinary name x y = pretty name <> parens (pretty x <> comma <+> pretty y)

-- | Abstract pretty.
pretty' :: Res -> String -> (a -> Res) -> [a] -> Res -> Res
pretty' l _ _ [] r   = l <+> r
pretty' l sym f dt r = group $ encloseSep (l <> space) (space <> r) (pretty sym) $ map f dt

-- | Pretties byte string.
prettyBytes :: ByteString -> Res
prettyBytes bytes = pretty' (pretty "[#") " " byte' unpacked $ pretty "#]"
  where
    unpacked = unpack bytes
    byte' byte
      | byte < 16 = pretty "0" <> pretty (showHex byte "")
      | otherwise = pretty $ showHex byte ""

-- | Pretties number.
prettyNumber :: Rational -> Res
prettyNumber number@(a :% b) = case c of
  Just _ -> case l of
    0 -> pretty r <> slash <> pretty b
    _ -> pretty l <+> pretty (if signum r == 1 then "+" else "-")
                  <+> pretty (abs r) <> slash <> pretty b
  Nothing ->
    let ss = floatingOrInteger s :: (Either Double Integer) in case ss of
      Left  x -> pretty $ showFFloat Nothing x ""
      Right y -> pretty y
  where
    (s, c) = fromRationalRepetendUnlimited number
    (l, r) = quotRem a b
