# Lisy

Simple repl with Hindley-Milner type system implementing the paper [Typing Haskell in Haskell](http://web.cecs.pdx.edu/~mpj/thih/thih.pdf).

```
Lisy> id a = a
()
Lisy> show (id '')
show (id '') : Show ([Char]) => [Char]
```

### Features

- Kinds in types
- Adhoc polymorphism
- W Algorithm type inference
