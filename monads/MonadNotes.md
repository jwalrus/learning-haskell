# Notes on Monads

### Definition
```haskell
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  fail :: String -> m a
```

* _note_ - `return` and applicative's `pure` are the same and are different only for historical reasons (applicatives were added to the language after monads)
* `>>` is useful for chaining methods with side effects; the first `m a` is effectively thrown away (e.g., `putStrLn` prints to the console and returns `()` which can just be thrown away)

### Notes
* monads extend applicatives similar to how applicatives extend functors
* list comprehensions in haskell are just simplified syntax for the list monad

### guard and the Alternative class
* `guard` can be used as a filter with monads
* signature: `guard :: Alternative f => Bool -> f ()
* Alternative is a subclass of Applicative (all Alternatives are Applicatives), but not all Monads are subsclasses of Alternative (List and Maybe are instances of Alternative, but IO is not)
* _todo_ revisit Alternatives after getting more comfortable with monads
