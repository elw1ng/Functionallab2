module Main where

import Prelude
import Data.Tuple(Tuple(..))
import Data.List(List(..),(:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex f l = findIndexCount f l 0
  where
  findIndexCount :: (a -> Boolean) -> List a -> Int -> Maybe Int
  findIndexCount _ Nil _ = Nothing
  findIndexCount f (x:xs) n | f x = Just n
    | otherwise = findIndexCount f xs (n+1)


findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex f l = findLastIndexCount f l 0 Nothing
  where
  findLastIndexCount :: (a -> Boolean) -> List a -> Int -> Maybe Int -> Maybe Int
  findLastIndexCount _ Nil _ r = r
  findLastIndexCount f (x:xs) n r | f x = findLastIndexCount f xs (n+1) (Just n)
    | otherwise = findLastIndexCount f xs (n+1) r


zip:: forall a b. List a -> List b -> List (Tuple a b)
zip (x:xs) (y:ys) = (Tuple x y) : (zip xs ys)
zip _ _ = Nil


unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip lst = go (Tuple Nil Nil) lst
  where
    go:: Tuple (List a) (List b) -> List (Tuple a b) -> Tuple (List a) (List b)
    go (Tuple acc1 acc2) (Cons (Tuple x y) lst) = go (Tuple (x :acc1) (y:acc2)) lst
    go (Tuple acc1 acc2) _ = (Tuple (rev Nil acc1) (rev Nil acc2))
      where
        rev :: forall c.List c -> List c-> List c
        rev acc Nil = acc
        rev acc (x : xs) = rev (x : acc) xs


filter :: forall a. (a -> Boolean) -> List a -> List a
filter _ Nil = Nil
filter f (x:xs) | f x =  x :(filter f xs)
              | otherwise = filter f xs


filtertail :: forall a. (a -> Boolean) -> List a -> List a
filtertail f l = go f l Nil
  where
  go :: (a -> Boolean) -> List a -> List a -> List a
  go _ Nil l = rev Nil l
    where
     rev :: List a -> List a -> List a
     rev acc Nil = acc
     rev acc (x : xs) = rev (x : acc) xs

  go f (x:xs) acc | f x =  go f xs (x:acc)
                | otherwise = go f xs acc


take :: forall a. Int -> List a -> List a
take _ Nil = Nil
take 0 _ = Nil
take n (x:xs) = x: take (n-1) xs

taketail :: forall a. Int -> List a -> List a
taketail n l = go n l Nil
  where
  go :: Int -> List a -> List a -> List a
  go n (x:xs) accc | n>0  =  go (n-1) xs (x:accc)
  go _ _ l = rev Nil l
                where
                rev :: List a -> List a -> List a
                rev acc Nil = acc
                rev acc (x : xs) = rev (x : acc) xs


main :: Effect Unit
main = do
  log $ show $ "Result of findIndex"
  log $ show $ findIndex (\x -> x>10) (1:2:10:4:4:12:32:Nil)
  log $ show $ findIndex (\x -> x>10) (1:2:4:4:4:2:3:Nil)
  log $ show $ findIndex (\x -> x>10) (Nil)
  log $ show $ findIndex (\x -> x>10) (11:Nil)
  log $ show $ "Result of findLastIndex"
  log $ show $ findLastIndex (\x -> x>10) (1:2:10:4:4:12:32:Nil)
  log $ show $ findLastIndex (\x -> x>10) (1:2:4:4:4:2:3:Nil)
  log $ show $ findLastIndex (\x -> x>10) (Nil)
  log $ show $ findLastIndex (\x -> x>10) (11:Nil)
  log $ show $ "Result of zip+unzip"
  log $ show $ zip (1:2:3:4:Nil) ("2":"3":"4":"5":Nil)
  log $ show $ unzip ((Tuple 1 "2") : (Tuple 2 "3") : (Tuple 3 "4") : (Tuple 4 "5") : Nil)
  log $ show $ "Result of filter"
  log $ show $ filter (\x -> x>10) (1:2:10:4:4:12:32:Nil)
  log $ show $ filter (\x -> x>10) (1:2:10:4:4:1:3:Nil)
  log $ show $ "Result of filtertail"
  log $ show $ filtertail (\x -> x>10) (1:2:10:4:4:12:32:Nil)
  log $ show $ filtertail (\x -> x>10) (1:2:10:4:4:1:3:Nil)
  log $ show $ filtertail (\x -> x>10) (1:2:10:4:4:12:32:Nil)
  log $ show $ "Result of take"
  log $ show $ take 3 (1:2:10:4:4:1:3:Nil)
  log $ show $ take 0 (1:2:10:4:4:1:3:Nil)
  log $ show $ take 13 (1:2:10:4:4:1:3:Nil)
  log $ show $ "Result of taketail"
  log $ show $ taketail 3 (1:2:10:4:4:1:3:Nil)
  log $ show $ taketail 0 (1:2:10:4:4:1:3:Nil)
  log $ show $ taketail 13 (1:2:10:4:4:1:3:Nil)