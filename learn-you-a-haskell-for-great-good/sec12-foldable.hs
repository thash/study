import Data.Monoid
import qualified Data.Foldable as F

-- Prelude F> :t foldr
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- Prelude F> :t F.foldr
-- F.foldr :: F.Foldable t => (a -> b -> b) -> b -> t a -> b

-- sec7でやった木構造にFoldableを適用する
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

instance F.Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x           `mappend`
                             F.foldMap f r

testTree = Node 5
             (Node 3
                 (Node 1 EmptyTree EmptyTree)
                 (Node 6 EmptyTree EmptyTree)
             )
             (Node 9
                 (Node 8 EmptyTree EmptyTree)
                 (Node 10 EmptyTree EmptyTree)
             )


-- *Main> getAny $ F.foldMap (\x -> Any $ x == 3) testTree
-- True


