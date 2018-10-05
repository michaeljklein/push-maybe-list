module Lib where

import Data.Bits
import Data.Maybe
import Control.Monad


-- | The "natural" partition of a list of `Maybe`'s:
-- we group `Nothing`'s and `Just`'s and the outermost `Maybe`
-- marks which is contained.
partMaybes :: [Maybe a] -> [Maybe [Maybe a]]
partMaybes [] = []
partMaybes ~(x:xs) =
  case x of
    Nothing -> loopn xs
    ~(Just y) -> loopj [y] xs
  where
    loopn :: [Maybe a] -> [Maybe [Maybe a]]
    loopn [] = [Nothing]
    loopn ~(z:zs) = case z of
      Nothing -> loopn zs
      ~(Just w) -> Nothing : loopj [w] zs

    loopj :: [a] -> [Maybe a] -> [Maybe [Maybe a]]
    loopj js [] = [Just $ Just <$> reverse js]
    loopj js ~(z:zs) = case z of
      Nothing -> Just (Just <$> reverse js) : loopn zs
      ~(Just w) -> loopj (w : js) zs


-- | Convert the "natural" partition into a partition that
-- we can more easily push through the innermost `Maybe`:
-- replace `Nothing`'s with an empty lists and drop `Just`'s.
unMaybeParts :: [Maybe [Maybe a]] -> [[Maybe a]]
unMaybeParts [] = []
unMaybeParts ~(x:xs) = case x of
                         Nothing -> [] : loopj xs
                         ~(Just ys) -> ys : loopn xs
  where
    loopn [] = []
    loopn ~(_:zs) = [] : loopj zs

    loopj [] = []
    loopj [z] = case z of
                  ~(Just w) -> [w]
    loopj ~(Just z:_:zs) = z : [] : loopj zs

-- | @`unMaybeParts` . `partMaybes`@
partMaybe :: [Maybe a] -> [[Maybe a]]
partMaybe = unMaybeParts . partMaybes

-- | Reassociate the list and `Maybe`
dartMaybe :: [[Maybe a]] -> [Maybe [a]]
dartMaybe [] = []
dartMaybe ~(x:xs) = case x of
  [] -> Nothing : loopj xs
  ys -> Just ((\(~(Just y)) -> y) <$> ys) : loopn xs
  where
    loopn :: [[Maybe a]] -> [Maybe [a]]
    loopn [] = []
    loopn ~(_:zs) = Nothing : loopj zs

    loopj :: [[Maybe a]] -> [Maybe [a]]
    loopj [] = []
    loopj [z] = [Just $ (\(~(Just w)) -> w) <$> z]
    loopj ~(z:_:zs) = Just ((\(~(Just w)) -> w) <$> z) : Nothing : loopj zs

-- | @`dartMaybe` . `partMaybe`@
pushMaybe :: [Maybe a] -> [Maybe [a]]
pushMaybe = dartMaybe . partMaybe


-- Tests

-- | Convert from pushed preserving `Nothing`'s
fromPushed :: [Maybe [a]] -> [Maybe a]
fromPushed = concatMap $ maybe [Nothing] (fmap Just)

-- | Replace runs of multiple `Nothing`'s with single `Nothing`'s
simplifyNothings :: [Maybe a] -> [Maybe a]
simplifyNothings [] = []
simplifyNothings ~(x:xs) =
  case x of
    Nothing -> loop xs
    y -> y : simplifyNothings xs
  where
    loop [] = [Nothing]
    loop ~(z:zs) =
      case z of
        Nothing -> loop zs
        _ -> Nothing : z : simplifyNothings zs

-- | @`fromPushed` . `pushMaybe` == `simplifyNothings`@
fromPushedIsSimplified :: Eq a => [Maybe a] -> Bool
fromPushedIsSimplified = liftM2 (==) (fromPushed . pushMaybe) simplifyNothings

-- | Drop the `Nothing`'s and flatten the resulting list
flattenPushed :: [Maybe [a]] -> [a]
flattenPushed = concatMap $ join . maybeToList

-- | @`flattenPushed` . `pushMaybe` == `catMaybes`@
flattenPushedIsCatMaybes :: Eq a => [Maybe a] -> Bool
flattenPushedIsCatMaybes = liftM2 (==) (flattenPushed . pushMaybe) catMaybes



-- Test helpers

-- | To a reversed list of he binary digits of the input
toBin :: (Bits a, Eq a, Enum a) => a -> [a]
toBin x =
  if x == toEnum 0
    then [toEnum 0]
    else if x == toEnum 1
           then [toEnum 1]
           else (x .&. toEnum 1) : toBin (x `unsafeShiftR` 1)

-- | If `False` then `Nothing` else `Just`, zipped
toMaybes :: [Bool] -> [a] -> [Maybe a]
toMaybes =
  zipWith $ \x y ->
    if x
      then Just y
      else Nothing

-- | Use `toBin` to get the list of `Bool`'s and use @[1..]@ as
-- the list of values. The first argument is the length and the
-- second is the index.
enumMaybes :: (Bits a, Eq a, Enum a) => Int -> a -> [Maybe a]
enumMaybes n x =
  toMaybes (take n $ fmap (== toEnum 0) (toBin x) ++ repeat False) [toEnum 0 ..]

-- | Apply a test to `enumMaybes` up to the given maximums.
-- Return any counterexamples.
testEnumMaybes :: ([Maybe Int] -> Bool) -> Int -> Int -> [[Maybe Int]]
testEnumMaybes p x y =
  [xs | i <- [0 .. x], j <- [0 .. y], let xs = enumMaybes i j, not $ p xs]

-- | Throw an error if the test fails on any input `enumMaybes`
assertEnumMaybes :: String -> ([Maybe Int] -> Bool) -> Int -> Int -> IO ()
assertEnumMaybes name p x y = do
  putStrLn $ concat ["Test name: ", name]
  case testEnumMaybes p x y of
    [] -> do
      putStrLn $
        concat
          ["Passed test with i <- [0..", show x, "], j <- [0..", show y, "]"]
      putStrLn ""
    xs -> error $ concat ["Test failed on: ", show xs]

runTests :: IO ()
runTests = do
  assertEnumMaybes "fromPushedIsSimplified" fromPushedIsSimplified 100 100
  assertEnumMaybes "flattenPushedIsCatMaybes" flattenPushedIsCatMaybes 100 100
  putStrLn "All tests passed!"


-- | Notes on digraphs:
--
-- @
--  newtype Digraph = Digraph { digraphMatrix :: Matrix Bool }
--
--  data DigraphGen = DigraphGen
--    { currentDigraph :: Digraph
--    , currentArc :: Arc
--    , newDigraph :: Bool
--    }
--
--  We can enumerate the non (i, i) arcs in (join liftM2 (,) [1..n])
--
--  this gives us an Enum instance for (ArcN (n :: Nat))
--
--  newtype ArcN (n :: Nat) = ArcN { toArc :: Arc }
-- @
--
digraphNotes :: ()
digraphNotes = ()

