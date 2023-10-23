module Set
  ( (<:>),
    (<++>),
    (<\\>),
    isSubsetOf,
    union,
    unique,
  )
where

import Data.Set ((\\))
import qualified Data.Set as S

unique :: (Ord a) => [a] -> [a]
unique = S.toList . S.fromList

(<:>) :: (Ord a) => a -> [a] -> [a]
x <:> l = unique (x : l)

(<++>) :: (Ord a) => [a] -> [a] -> [a]
l1 <++> l2 = unique (l1 ++ l2)

(<\\>) :: (Ord a) => [a] -> [a] -> [a]
l1 <\\> l2 = S.toList (S.fromList l1 \\ S.fromList l2)

isSubsetOf :: (Ord a) => [a] -> [a] -> Bool
isSubsetOf l1 l2 = S.fromList l1 `S.isSubsetOf` S.fromList l2

union :: (Ord a) => [a] -> [a] -> [a]
union l1 l2 = S.toList (S.fromList l1 `S.union` S.fromList l2)

infixr 5 <:>

infixr 5 <++>
