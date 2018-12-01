module WAT where

import qualified Tokens as Tk

import qualified Data.Text as Tx
import qualified Data.List as List

data Label
  = Module
  | Func
  | Param
  | Result
  | Export Tx.Text -- Split?
  deriving Show

data Value
  = I32
  | I64
  | F32
  | F64
  | Name Tx.Text
  | GetLocal Int
  | GetLocalByName Tx.Text
  | I32Add
  deriving Show

data Expression
  = Leaf Value
  | Node Label [Expression]
  deriving Show

node :: Label -> [Value] -> Expression
node label values
  = Node label (Leaf <$> values)

tokenize :: Expression -> [Tk.Token]
tokenize expression
  = case expression of
      Leaf value          -> [tokenizeValue value]
      Node label children -> tokenizeNode label children

tokenizeValue :: Value -> Tk.Token
tokenizeValue value
  = case value of
      I32                 -> Tk.I32
      I64                 -> Tk.I64
      F32                 -> Tk.F32
      F64                 -> Tk.F64
      Name name           -> Tk.Name name
      GetLocal i          -> Tk.GetLocal i
      GetLocalByName name -> Tk.GetLocalByName name
      I32Add              -> Tk.I32Add

tokenizeNode :: Label -> [Expression] -> [Tk.Token]
tokenizeNode label children
  = Tk.Open
  : tokenizeLabel label
  : Tk.Space
  : List.intercalate [Tk.Space] (tokenize <$> children)
  ++ [Tk.Close]

tokenizeLabel :: Label -> Tk.Token
tokenizeLabel label
  = case label of
      Module      -> Tk.Module
      Func        -> Tk.Func 
      Param       -> Tk.Param 
      Result      -> Tk.Result
      Export name -> Tk.Export name
