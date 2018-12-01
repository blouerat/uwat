{-# LANGUAGE RecordWildCards #-}

module Lib where

import qualified WAT
import qualified Tokens as Tk

import Data.Text as Tx

data Type
  = I32
  | I64
  | F32
  | F64
  deriving Show

data Instruction
  = GetLocalByName Tx.Text
  | I32Add
  deriving Show

data Parameter
  = Parameter
      { _pName :: Tx.Text
      , _pType :: Type
      }
  deriving Show

data Function
  = Function
      { _fName   :: Maybe Tx.Text
      , _fParams :: [Parameter]
      , _fResult :: Maybe Type
      , _fBody   :: [Instruction]
      }
  deriving Show

data Export
  = Export
      { _eExportName    :: Tx.Text
      , _eFunctionName  :: Tx.Text
      }
  deriving Show

data AST
  = Module [AST]
  | Func Function
  | Exp Export
  deriving Show

function :: Tx.Text -> [(Tx.Text, Type)] -> Type -> [Instruction] -> AST
function name params result body
  = Func (Function (Just name) (uncurry Parameter <$> params) (Just result) body)

push :: Tx.Text -> Instruction
push
  = GetLocalByName

export :: Tx.Text -> AST
export name
 = Exp (Export name name)

desugar :: AST -> WAT.Expression
desugar ast
  = case ast of
      Module content  -> WAT.Node WAT.Module (desugar <$> content)
      Func f          -> desugarFunction f
      Exp e           -> desugarExport e

desugarFunction :: Function -> WAT.Expression
desugarFunction Function{..}
  = WAT.Node WAT.Func (name ++ params ++ result ++ body)
  where
    name
      = foldMap toName _fName

    toName n
      = [WAT.Leaf (WAT.Name n)]

    params
      = toParam <$> _fParams

    toParam Parameter{..}
      = WAT.node WAT.Param [WAT.Name _pName, toType _pType]

    toType t
      = case t of
          I32 -> WAT.I32
          I64 -> WAT.I64
          F32 -> WAT.F32
          F64 -> WAT.F64

    result
      = foldMap toResult _fResult

    toResult t
      = [WAT.node WAT.Result [toType t]]

    body
      = toInstruction <$> _fBody

    toInstruction i
      = WAT.Leaf $ case i of
          GetLocalByName n  -> WAT.GetLocalByName n
          I32Add            -> WAT.I32Add

desugarExport :: Export -> WAT.Expression
desugarExport Export{..}
  = WAT.Node (WAT.Export _eExportName) [WAT.node WAT.Func [(WAT.Name _eFunctionName)]]

tokenize :: AST -> [Tk.Token]
tokenize
  = WAT.tokenize . desugar

serialize :: AST -> Tx.Text
serialize
  = Tk.serialize . tokenize
