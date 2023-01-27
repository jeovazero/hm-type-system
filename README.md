## hindley-milner type system

using simple haskell

```
$ runhaskell -ilib examples/Main.hs
False 
-------------------
Infer: "Bool" 

True 
-------------------
Infer: "Bool" 

Just False 
-------------------
Infer: "Maybe" "Bool" 

Nothing 
-------------------
Infer: "Maybe" a

let
  id = \x -> x
in id
-------------------
Infer: (a -> a)

let
  fix = \f -> f (fix (f))
in fix
-------------------
Infer: ((a -> a) -> a)
```
