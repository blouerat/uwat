{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import qualified Data.Text.IO as TxIO

ast :: AST
ast
  = Module
      [ function
          "add"

          [ ("lhs", I32)
          , ("rhs", I32)
          ]
          
          I32
          
          [ push "lhs"
          , push "rhs"
          , I32Add
          ]

      , export "add"
      ]

main :: IO ()
main = TxIO.putStrLn . serialize $ ast
