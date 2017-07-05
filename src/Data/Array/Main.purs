module Data.Array.Shuffle (
    shuffle
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff, whileE)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.ST (ST, modifySTRef, newSTRef, readSTRef, runST)
import Data.Array (catMaybes, index, length, mapWithIndex)
import Data.Array.ST (STArray, emptySTArray, freeze, peekSTArray, pokeSTArray, pushSTArray, thaw, unsafeFreeze)
import Data.Maybe (Maybe(..))
import Data.Ord (lessThanOrEq)
import Data.Tuple (Tuple(..), fst, snd)

-- | Randomly shuffle an Array of items using the
-- | fisher-yates shuffle algorithm
shuffle :: forall eff a. Array a -> Eff (random :: RANDOM | eff) (Array a)
shuffle orig = runST (mutate >>= unsafeFreeze)
  where
    indexJusts :: forall b. Array (Maybe b) -> Array (Tuple Int b)
    indexJusts = catMaybes <<< mapWithIndex \i -> case _ of
      Just v -> Just (Tuple i v)
      Nothing -> Nothing
    
    -- | Return an element at an index.  If the index is greater than
    -- | the length of the array, wrap to the beginning of the array
    wrappingIndex :: forall b. Array b -> Int -> Maybe b
    wrappingIndex [] _ = Nothing
    wrappingIndex a  i = index a (mod i $ max ((length a) - 1) 1)

    mutate :: forall e h. Eff (random :: RANDOM, st :: ST h | e) (STArray h a)
    mutate = do
      count       <- newSTRef 0 
      origIndexed <- thaw (map (\v -> Just v) orig)
      result      <- emptySTArray
      
      let arrayUpperBound = (length orig) - 1

      let resultIncomplete =
            readSTRef count >>=
            flip lessThanOrEq arrayUpperBound >>>
            pure

      whileE resultIncomplete do
        i <- randomInt 0 arrayUpperBound
        peekSTArray origIndexed i >>= case _ of 
          Just randomElement1 -> do
            remaining <- freeze origIndexed >>= indexJusts >>> pure
            let randomElement2 = wrappingIndex remaining i            

            case (Tuple i <$> randomElement1 <|> randomElement2 ) of
              Just v -> do
                void $ pokeSTArray origIndexed (fst v) Nothing
                void $ pushSTArray result (snd v)
                void $ modifySTRef count (add 1)
                pure unit 
              Nothing -> pure unit
          Nothing -> pure unit      
      pure result