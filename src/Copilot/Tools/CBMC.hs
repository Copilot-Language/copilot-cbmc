--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Copilot.Tools.CBMC (Params (..), defaultParams, genCBMC) where

import Copilot.Core
import qualified Copilot.Compile.C99 as C99
import qualified Copilot.Compile.SBV as SBV
import qualified System.IO as I
import Text.PrettyPrint.HughesPJ

data Params = Params
  { numIterations :: Int }

defaultParams :: Params
defaultParams = Params
  { numIterations = 10 }

genCBMC :: Params -> Spec -> IO ()
genCBMC params spec =
  do
    C99.compile (C99.defaultParams { C99.prefix = Just "atm" }) spec
    SBV.compile (SBV.defaultParams { SBV.prefix = Just "sbv" }) spec
    h <- I.openFile "cbmc_driver.c" I.WriteMode
    I.hPutStrLn h (render (driver params spec))

driver :: Params -> Spec -> Doc
driver Params { numIterations = k } spec = vcat
  [ text "#include <stdbool.h>"
  , text "#include <stdint.h>"
  , text "#include \"atm_copilot.h\""
  , text "#include \"sbv_copilot/sbv_copilot.h\""
  , text ""
  , text "int32_t nondet_bool();"
  , text "int32_t nondet_uint8_t();"
  , text "int32_t nondet_uint16_t();"
  , text "int32_t nondet_uint32_t();"
  , text "int32_t nondet_uint64_t();"
  , text "int32_t nondet_int8_t();"
  , text "int32_t nondet_int16_t();"
  , text "int32_t nondet_int32_t();"
  , text "int32_t nondet_int64_t();"
  , text "int32_t nondet_float();"
  , text "int32_t nondet_double();"
  , text ""
  , declExterns spec
  , text ""
  , sampleExterns spec
  , text ""
  , verifyObservers spec
  , text ""
  , text "int main (int argc, char const *argv[])"
  , text "{"
  , text "  int i;"
  , text ""
  , text "  for (i = 0; i < " <> int k <> text "; i++)"
  , text "  {"
  , text "    sampleExterns();"
  , text "    atm_step();"
  , text "    sbv_step();"
  , text "    verify_observers();"
  , text "  }"
  , text ""
  , text "  return 0;"
  , text "}"
  ]

declExterns :: Spec -> Doc
declExterns = vcat . map declExtern . externVars

  where

    declExtern :: ExtVar -> Doc
    declExtern (ExtVar name t) = typeSpec t <+> text name <> semi

sampleExterns :: Spec -> Doc
sampleExterns spec = vcat
  [ text "void sampleExterns()"
  , text "{"
  , text ""
  , nest 2 (vcat $ map sampleExtern (externVars spec))
  , text "}"
  ]

  where

    sampleExtern :: ExtVar -> Doc
    sampleExtern (ExtVar name t) =
      text name <+> text "=" <+> text "nondet_" <> typeSpec t <> text "();"

verifyObservers :: Spec -> Doc
verifyObservers spec = vcat
  [ text "void verify_observers()"
  , text "{"
  , text ""
  , nest 2 (vcat $ map verifyObserver (specObservers spec))
  , text "}"
  ]

  where

    verifyObserver :: Observer -> Doc
    verifyObserver (Observer name _ _) =
      text "assert(" <> text "atm_" <> text name <+> text "==" <+> text "sbv_" <>
      text name <> text ");"

typeSpec :: UType -> Doc
typeSpec UType { uTypeType = t } = text (typeSpec' t)

  where

  typeSpec' Bool   = "bool"
  typeSpec' Int8   = "int8_t"
  typeSpec' Int16  = "int16_t"
  typeSpec' Int32  = "int32_t"
  typeSpec' Int64  = "int64_t"
  typeSpec' Word8  = "uint8_t"
  typeSpec' Word16 = "uint16_t"
  typeSpec' Word32 = "uint32_t"
  typeSpec' Word64 = "uint64_t"
  typeSpec' Float  = "float"
  typeSpec' Double = "double"
