Talloy is a not-very-serious attempt to make a strict impure Haskell with lightweight killable threads, generics, sum and product types, nominal types, and garbage collection, but with strict evaluation and no type-checked purity.

So far, it's capable of describing simple programs with `print`, `sleep`, and sequencing:

```
{
  print "hi";
  sleep 0.5;
  print "there";
}
```

### Development

Install [ghcid](https://github.com/ndmitchell/ghcid), then ruN:

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