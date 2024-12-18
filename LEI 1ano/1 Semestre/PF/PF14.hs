module Aula1410 where

data BTree = Empty
            | Node Int BTree BTree

bt :: BTree
bt = Node 5 (Node 7 Empty Empty)
            
            (Node 3 Empty
                    (Node 2 Empty Empty)
            )

--data TTree a = Leaf a
--              | Fork (TTree a) (TTree a) (TTree)

data RTree a = R a [RTree a]
            deriving Show

a :: RTree Int
a = R 3 [R 5 [R 1 []
             ,R 2 []
             ,R 3 []
             ,R 4 []]
    , R 7 [R 8 []]
    , R 4 []
    ]

empty :: RTree String
empty = R "Empty" []

node :: a -> RTree String -> RTree String -> RTree String -> RTree String
node i l r = R "Node"
                [ R (show i) []
                , l
                , r
                ]

btAsRTree = node 5 empty (node 4 empty empty)