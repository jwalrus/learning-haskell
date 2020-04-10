# Notes on Applicatives

### Recall - Functor
* `fmap :: Functor f => (a -> b) -> f a -> f b`
* applicatives generalize functor's `fmap` to work on multiple arguments
* for example, `maybeInc = (+) <$> Just 1` has a type of `Maybe (Integer -> Integer)` which is a function that cannot be called when limited to functors

### Applicatives

#### Definition

```haskell
class Functor f => Applicative f where
    pure :: a = f a
    (<*>) :: f (a -> b) -> f a -> f b

```

#### Laws
* `pure f <*> x = fmap f x`
* `pure id <*> v = v`
* `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`
* `pure f <*> pure x = pure (f x)`
* `u <*> pure y = pure ($ y) <*> u`

#### Notes
* `<*>` from Applicative allows us to use `maybeInt`. it allows us to apply a function _within_ a context, such as `Maybe`, `List`, etc.
* example: `(+) <$> Just 1 <*> Just 10` evaluates to `Just 11`
  * `(+) <$> Just 1` uses partial application to create a function within a context
  * `<*> Just 10` takes the previous function (in a context) and applies it to an argument in the same context
* partial application allows you to chain together as many arguments as you want (see [min3.hs](./min3.hs))
* note on container vs context
  * parametrized types that represent a container are types that represent a data structure
  * when a type is context, extra info is implied about type, beyond its structure (e.g., the "world" your computations are occuring in: you calculation _either_ has a failure or a success, your computation interacted with the real world, etc.)
  * functors are containers, applicatives are contexts

#### Case study: Lists
* List is interesting because it is both a container *and* a context
    * a list as a *container* is a sequence of values
    * a list as a *context* is a set of possibilities (a single variable that has many possible values), non-deterministic computing
* key question: "what does it mean to apply a function to two or more elements in the context of a list?" (e.q. `(+) <$> [1,2] <*> [3,4]`)
* see [primes.hs](./primes.hs) and [user.hs](./user.hs) for examples of using lists as contexts


