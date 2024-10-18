-- nums = [2,7,11,15], target = 9
-- output = [0,1] -> indexes -> 2 + 7 = 9
-- generate sums of each item with the next
import Data.List
import Data.Maybe (isNothing, isJust)

isPositive :: (Num a, Ord a) => (a, a) -> Bool
isPositive (a, b) = (a > 0) && (b > 0)

isTupleJust :: Num a => (Maybe a, Maybe a) -> Bool
isTupleJust (a, b) = isJust a && isJust b

tupleElemIndex :: (Num a, Eq a) => [a] -> (a, a) -> (Maybe Int, Maybe Int)
tupleElemIndex xs (a, b) = (elemIndex a xs, elemIndex b xs)

twoSum :: (Num a, Ord a) => a -> [a] -> (Maybe Int, Maybe Int)
twoSum target nums = head $ filter isTupleJust $ map (tupleElemIndex nums) $ filter isPositive $ zip nums $ map (target -) nums

main :: IO()
main = print (twoSum 9 [2,7,11,15])

