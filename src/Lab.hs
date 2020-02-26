--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Recursive data types                                                  --
--------------------------------------------------------------------------------

module Lab where

--------------------------------------------------------------------------------
-- Red-black trees

data Colour = Red | Black

instance Show Colour where
    show Red   = "Red"
    show Black = "Black"

data Tree a = Leaf | Node Colour (Tree a) a (Tree a)
    deriving Show

empty :: Tree a
empty = Leaf

singleton :: a -> Tree a
singleton a = Node Red Leaf a Leaf

makeBlack :: Tree a -> Tree a
makeBlack (Node _ l x r) = Node Black l x r

depth :: Tree a -> Int
depth Leaf           = 0
depth (Node _ l _ r) = 1 + max (depth l) (depth r)

toList :: Tree a -> [a]
toList Leaf           = []
-- toList (Node _ l x r) = x : (toList l) ++ (toList r) -- preorder
toList (Node _ l x r) = toList l ++ (x : toList r) -- inorder

member :: Ord a => a -> Tree a -> Bool
member _ Leaf = False
-- member a (Node _ l x r) = a == x || member a l || member a r
member a (Node _ l x r)
    | a == x    = True
    | a >  x    = member a r
    | otherwise = member a r

balance :: Colour -> Tree a -> a -> Tree a -> Tree a
balance Black (Node Red (Node Red a x b) y c) z d =
    Node Red (Node Black a x b) y (Node Black c z d)
balance Black (Node Red a x (Node Red b y c)) z d =
    Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red b y (Node Red c z d)) =
    Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red (Node Red b y c) z d) =
    Node Red (Node Black a x b) y (Node Black c z d)
balance c l x r = Node c l x r

balance' (Node c l x r) = balance c l x r

insert :: Ord a => Tree a -> a -> Tree a
insert tree a = makeBlack $ insert' tree a
    where insert' :: Ord a => Tree a -> a -> Tree a
          insert' Leaf           a = singleton a
          insert' (Node c l x r) a
              | a == x    = Node c l x r
              | a >  x    = balance c l x (insert' r a)
              | otherwise = balance c (insert' l a) x r

--------------------------------------------------------------------------------
