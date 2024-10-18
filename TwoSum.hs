-- nums = [2,7,11,15], target = 9
-- output = [0,1] -> indexes -> 2 + 7 = 9
-- generate sums of each item with the next
import Data.List

twoSum :: Num a => a -> [a] -> (Maybe Int, Maybe Int)
twoSum =
    let a = map (target -) nums
        b = zip nums a
        c = filter (\(x,y) -> (x > 0) && (y > 0)) b
        d = head c
    in (elemIndex (fst d) nums, elemIndex (snd d) nums)

nums = [2,7,11,15]
target = 9

main :: IO()
main = do
    print (twoSum 9 nums)

