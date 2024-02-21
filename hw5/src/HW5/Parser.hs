{-
In this file implemented logic for parser.
-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}

module HW5.Parser
  ( parse
  ) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)

import Numeric         (readHex)
import Data.Char       (isAlpha, isAlphaNum)
import Data.List       (intercalate)
import Data.Void       (Void)
import Data.Functor    (($>))

import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char

import qualified Data.Text                  as DText
import qualified Text.Megaparsec.Char.Lexer as DLexer
import qualified Data.ByteString            as DByte

import HW5.Utility
import HW5.Base

-- | Type alias for Parser.
type Parser = Parsec Void String

-- | Function for string parsing, returns either HiExpr result or error.
parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (space *> mainExpr <* eof) mempty

-- | Main parser, parses brackets and uses opMapping for operator forms.
mainExpr :: Parser HiExpr
mainExpr = makeExprParser brackets' opMapping

-- | Level for parsing dot access after brackets, function call after brackets or IO call.
brackets' :: Parser HiExpr
brackets' = brackets >>= \inp -> optional (pStr "(") >>= \case
  Just _  -> HiExprApply inp <$> seq' <* pStr ")"
  Nothing -> optional (pStr ".") >>= \case
    Just _  -> dotNames >>= (return . HiExprApply inp) . pure
    Nothing -> ioLevel $ pure inp

-- | Parses or expression in brackets or applies function to it arguments.
brackets :: Parser HiExpr
brackets = optional (pStr "(") >>= \case
  Just _  -> mainExpr <* pStr ")"
  Nothing -> valuesLevel >>= applyOrExpr . return

-- | Application or some value (see valuesLevel).
applyOrExpr :: Parser HiExpr -> Parser HiExpr
applyOrExpr inp = optional args' >>= \case
  Nothing   -> ioLevel inp
  Just args -> ioLevel (HiExprApply <$> inp <*> pure args) >>= applyOrExpr . return

-- | IO level.
ioLevel :: Parser HiExpr -> Parser HiExpr
ioLevel expr = optional (pStr "!") >>= \case
  Nothing -> expr
  Just _  -> fmap HiExprRun expr

-- | Values level.
valuesLevel :: Parser HiExpr
valuesLevel = HiExprValue <$> (  HiValueNumber             <$>
                                 DLexer.signed space (toRational <$> DLexer.lexeme space DLexer.scientific)
                             <|> HiValueFunction           <$> fun'
                             <|> HiValueBool               <$> bool'
                             <|> HiValueNull               <$  pStr "null"
                             <|> HiValueString             <$> string''
                             <|> HiValueBytes              <$> bytes'
                             <|> HiValueAction HiActionCwd <$  pStr "cwd"
                             <|> HiValueAction HiActionNow <$  pStr "now"
                              )
                          <|> HiExprApply (HiExprValue $ HiValueFunction HiFunList) <$> list'
                          <|> HiExprDict <$> dict'
  where
    dict'    = pStr "{" *> sepBy pair (pStr ",") <* pStr "}"
    pair     = try $ mainExpr >>= \ex1 -> pStr ":" >> mainExpr >>= \ex2 -> return (ex1, ex2)
    bytes    = fmap DByte.pack (sepEndBy byte space1)
    byte     = fst . head . readHex <$> count 2 hexDigitChar
    bytes'   = pStr "[#" *> bytes <* pStr "#]"
    list'    = pStr "[" *> seq' <* pStr "]"
    bool'    = True <$ pStr "true" <|> pStr "false" $> False
    string'' = DText.pack <$> DLexer.lexeme space (char '"' *> manyTill DLexer.charLiteral (char '"'))

-- | Parser that chooses functions.
fun' :: Parser HiFun
fun' = choice $ map (\fun -> pStr (getToken fun) $> fun)
  [HiFunDiv,        HiFunMul,         HiFunAdd,            HiFunAnd,         HiFunOr,          HiFunLessThan,
   HiFunEquals,     HiFunNotLessThan, HiFunNotGreaterThan, HiFunNotEquals,   HiFunIf,          HiFunGreaterThan,
   HiFunLength,     HiFunToUpper,     HiFunToLower,        HiFunReverse,     HiFunTrim,        HiFunList,
   HiFunRange,      HiFunFold,        HiFunPackBytes,      HiFunUnpackBytes, HiFunEncodeUtf8,  HiFunRand,
   HiFunDecodeUtf8, HiFunZip,         HiFunUnzip,          HiFunSerialise,   HiFunDeserialise, HiFunNot,
   HiFunRead,       HiFunWrite,       HiFunMkDir,          HiFunChDir,       HiFunParseTime,   HiFunSub,
   HiFunEcho,       HiFunCount,       HiFunKeys,           HiFunValues,      HiFunInvert]

-- | MainExpr parsers divided by comma.
seq' :: Parser [HiExpr]
seq' = sepBy mainExpr (pStr ",")

-- | Parses args or dot access call.
args' :: Parser [HiExpr]
args' = char '.' *> fmap pure dotNames <|> pStr "(" *> seq' <* pStr ")"

-- | Parser for parsing names after dot (expression was taken from FAQ).
dotNames :: Parser HiExpr
dotNames = HiExprValue . HiValueString . DText.pack . intercalate "-" <$>
           (((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-') <* space

-- | Mapping for operator forms.
opMapping :: [[Operator Parser HiExpr]]
opMapping =
  [[InfixL $ bOp HiFunMul         <$ pStr "*" , InfixL $ bOp HiFunDiv            <$ dup  "/"],
   [InfixL $ bOp HiFunAdd         <$ pStr "+" , InfixL $ bOp HiFunSub            <$ pStr "-"],
   [InfixN $ bOp HiFunEquals      <$ pStr "==", InfixN $ bOp HiFunNotEquals      <$ pStr "/=",
    InfixN $ bOp HiFunLessThan    <$ dup  "<" , InfixN $ bOp HiFunNotGreaterThan <$ pStr "<=",
    InfixN $ bOp HiFunGreaterThan <$ dup  ">" , InfixN $ bOp HiFunNotLessThan    <$ pStr ">="],
   [InfixR $ bOp HiFunAnd         <$ pStr "&&"],
   [InfixR $ bOp HiFunOr          <$ pStr "||"]]
  where
    bOp op a b = HiExprApply (HiExprValue $ HiValueFunction op) [a, b] -- just simple binary operator
    dup s = DLexer.lexeme space $ try $ pStr s <* notFollowedBy (pStr "=") -- special string parser for >, >=, <, <=, =, /=

-- | Parses string.
pStr :: String -> Parser String
pStr = DLexer.symbol space
