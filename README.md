# purescript-array-shuffle

## Module Data.Array.Shuffle

#### `shuffle`

``` purescript
shuffle :: forall eff a. Array a -> Eff (random :: RANDOM | eff) (Array a)
```

Randomly shuffle an Array of items using the fisher-yates shuffle algorithm.