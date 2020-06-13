Talloy is a not-very-serious attempt to make a strict impure Haskell with lightweight killable threads, generics, sum and product types, nominal types, and garbage collection, but with strict evaluation and no type-checked purity.

- The interpreter is currently written in Haskell
- Strict evaluation comes from sequencing the interpreter evaluation in `do` notation
- Impurity comes from omitting `IO` from functions that interact with the world
- Lightweight killable threads will come from piggybacking on the GHC runtime
- Generics are TODO
- Sum and product types are TODO
- Nominal types are TODO
- Garbage collection is TODO

So far, it's capable of describing simple programs with `print`, `sleep`, and sequencing:

```
{
  print "hi";
  sleep 0.5;
  print "there";
}
```

Output:

```
hi
there
```

### Development

Install [ghcid](https://github.com/ndmitchell/ghcid), then run:

```
./dev
```

Other ideas that might be interesting to throw in the mix:

- Overloaded values, disambiguated by type at call site
- No type classes, only interfaces
- Wwap out implementation of interfaces at call site implicitly (dynamic scope?)
- Everything is Showable, DeepSeqable, Hashable, etc.
- Stop parsing at blank line to avoid inscrutable parse errors
- Top level of file is implicit do block like Python, but you can refer to things out of order

### Overloading functions at the call site

```haskell
let { greet = print; } in dynlet { print arg = primPrint "blah"; } in greet "julien" -- blah
let { greet = print; } in let    { print arg = primPrint "blah"; } in greet "julien" -- julien

(let
  (greet (lambda (arg) (print arg)))
  (dynlet
    (print (lambda (arg) (primPrint "blah")))
    (greet "julien"))) -- prints blah

(let
  (greet (lambda (arg) (print arg)))
  (let
    (print (lambda (arg) (primPrint "blah")))
    (greet "julien"))) -- prints julien

fn = dynlet { print = missiles } in ...

{
  fn;
  print "HEY"; -- missles
}
```
