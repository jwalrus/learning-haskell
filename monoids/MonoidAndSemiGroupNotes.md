# Monoids and Semigroups

### Definitions
```haskell
class Semigroup a where
    (<>) :: a -> a -> a

class Monoid a where
    mempty :: a
    mappend :: a -> a -> a
    mconcat :: [a] -> a
```

### Applicative Laws
1. associativity: `a <> (b <> c) = (a <> b) <> c`

### Monoid Laws
1. identity: `mappend mempty x = x`
2. identity: `mappend x mempty = x`
3. associativity: `mappend x (mappend y z) = mappend (mappend x y) z`
4. definitiono of mconcat: `mconcat = foldr mappend mempty`

### Notes
* Semigroups live in `Data.Semigroup`
* Semigroups are about composability, and the `<>` operator is used to combine instances of the same type
* type signature: `(<>) :: Semigroup a => a -> a -> a`
* Semigroups should be associative: `(a <> b) <> c == a <> (b <> c)`
* see [Colors.hs](.\Colors.hs) for example of semigroup in practice
* Monoids are Semigroups with identity
* Semigroups where created after Monoids, while not the definition in haskell, a reasonable, intuitive definition of monoid is:
```haskell
class Semigroup a => Monoid a where
    identity :: a
```
* only `mempty` and `mappend` are -required- for Monoid. If those are defined, then haskell can infer a default implementation of `mconcat`
* see [ProbabilityTable.hs](.\ProbabilityTable.hs) for example of monoid in practice
