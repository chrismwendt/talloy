Talloy is a not-very-serious attempt to make a strict impure Haskell with lightweight killable threads, generics, sum and product types, nominal types, and garbage collection, but with strict evaluation and no type-checked purity.

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

### Roadmap

- [x] `let { ... } in ...`
- [x] `override { ... } in ...`
- [x] Lambdas
- [x] Printing to stdout
- [ ] Addition
- [ ] Recursion
- [ ] Sum and product types
- [ ] Nominal types
- [ ] Generics
- [ ] Garbage collection
- [ ] Lightweight killable threads
- [ ] Compile to C
- [ ] Self host
- [ ] Run Crysis

### Development

Install [ghcid](https://github.com/ndmitchell/ghcid), then run:

```
./dev
```

Other ideas that might be interesting to throw in the mix:

- Overloaded values, disambiguated by type at call site
- No type classes, only interfaces
- Swap out implementation of interfaces at call site implicitly (dynamic scope?)
- Everything is Showable, DeepSeqable, Hashable, etc.
- Stop parsing at blank line to avoid inscrutable parse errors
- Top level of file is implicit do block like Python, but you can refer to things out of order

### Overloading functions at the call site

```haskell
let { greet = \name -> print name; }
in override { print:oldprint = \str -> oldprint (uppercase str); }
in greet "john doe"
```
