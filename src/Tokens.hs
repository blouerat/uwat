{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Tokens where

import qualified Data.Text as Tx

data Token
  = Open
  | Close
  | Space
  | Module
  | Func
  | Param
  | Name Tx.Text
  | Result
  | I32
  | I64
  | F32
  | F64
  | GetLocal Int
  | GetLocalByName Tx.Text -- Split?
  | I32Add
  | Export Tx.Text -- Split?
  deriving Show

serialize :: [Token] -> Tx.Text
serialize
  = foldMap $ \case
      Open                -> "("
      Close               -> ")"
      Space               -> " "
      Module              -> "module"
      Func                -> "func"
      Param               -> "param"
      Name name           -> "$" <> name
      Result              -> "result"
      I32                 -> "i32"
      I64                 -> "i64"
      F32                 -> "f32"
      F64                 -> "f64"
      GetLocal i          -> "get_local " <> Tx.pack (show i)
      GetLocalByName name -> "get_local $" <> name
      I32Add              -> "i32.add"
      Export name         -> "export \"" <> name <> "\""
