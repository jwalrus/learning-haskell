# Functors

### Definition
```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

### Functor Laws
1. identity: `fmap id = id`
2. composition and associativity: `fmap (f . g) x = fmap f (fmap g x)`

### Notes
* `<$>` is the binary operator for `fmap` (the official name of the function)
  * `fmap (+3) Just 1 = 4`
  * `(+3) <$> Just 1 = 4`
