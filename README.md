## hindley-milner type system

using simple haskell

```hs
-- app/Main.hs

main = do
  runInfer $ letrec ["id"] [lam "x" (var "x")] (var "id")
  runInfer $ letrec ["fix"] [lam "f" (ap (var "f") (ap (var "fix") (var "f")))] (var "fix")
  runInfer $ letrec ["id"] [lam "x" (var "x")] (ap (var "id") (var "id"))
  runInfer $ letrec ["id"] [lam "x" (var "x")] (ap (var "id") (litInt 10))
```

```bash
$ runhaskell -ilib app/Main.hs
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

let
  id = \x -> x
in id (id)
-------------------
Infer: (a -> a)

let
  id = \x -> x
in id (10)
-------------------
Infer: Int
```
