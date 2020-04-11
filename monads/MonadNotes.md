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
* monads extend applicative similar to how applicatives extend functors

