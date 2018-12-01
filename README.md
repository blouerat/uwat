# uwat

Replacement for Javascript

```
Module
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
```
