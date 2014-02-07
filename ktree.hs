-- Limits and Applications of Group Algebras for Parameterized Problems
-- Ioannis Koutis and Ryan Williams

import Data.List
import Data.Tree

type Graph a = [(a,a)]
type ArCir = String

prod :: [ArCir] -> ArCir
prod factors = intercalate "." factors

sigma :: [ArCir] -> ArCir
sigma terms = intercalate "+" terms

nit :: Tree a -> a
nit (Node val children) = val

-- Theorem 1. The k-Tree problem can be solved in O*(2^k) time.

-- Definition of arithmetic circuit
c :: Graph Integer -> Tree Integer -> Integer -> Integer -> ArCir
c graph (Node root []) i j = "x" ++ (show j)

c graph (Node root children) i j =
    prod
        [(sigma ["x"++show(j)++"("++(c graph child (nit child) jprime)++")"
                    | (jj, jprime) <- graph, jj == j])
            | child <- children]

q :: Graph Integer -> Tree Integer -> Integer -> Integer -> [ArCir]
q graph tree k n = [c graph tree i j | i <- [1..k], j <- [1..n]]

-- Voorbeeld oproepen
-- Hetzelfde voorbeeld als in de email
-- boom
-- 1
-- |-2
-- `-3
-- grafe
-- 1 - 2
-- | \ |
-- 4   3
--
-- q [(1,2),(1,3),(1,4),(2,1),(2,3),(3,1),(3,2),(3,4),(4,1),(4,3)] (Node 1 [Node 2 [], Node 3 []]) 3 4

-- Voor dit voorbeeld zijn er wel termen die geen kwadraat bevatten
-- boom
-- 1
-- `-2
--   `-3
-- grafe
-- 1-2-3
--
-- q [(1,2),(2,1),(2,3),(3,2)] (Node 1 [Node 2 [Node 3 []]]) 3 3
