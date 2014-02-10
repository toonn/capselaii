-- Limits and Applications of Group Algebras for Parameterized Problems
-- Ioannis Koutis and Ryan Williams

import Data.List
import Data.Tree

type Graph a = [(a,a)]
type ArCir = String

prod :: [ArCir] -> ArCir
prod factors = intercalate "." factors

sigma :: [ArCir] -> ArCir
sigma terms = intercalate " + " terms

-- Theorem 2. The k-Leaf Spanning Tree problem can be solved in O*(2^k) time.

-- Definition of arithmetic circuit
c :: Graph String -> Integer -> Integer -> String -> ArCir
c graph 1 1 i = "(x" ++ i ++ ".z)"
c graph k t i
    | t < k || k <= 0 = "1"
    | otherwise = sigma [
                    sigma [
                        sigma [
                            prod ["C_" ++ (show k') ++ "," ++ (show t') ++ ","
                                        ++ j,
                                   "C_" ++ (show $ k-k') ++ ","
                                        ++ (show $ t-t') ++ "," ++ i ]
                            | (i_,j) <- graph, i_ == i]
                        | k' <- [1..k]]
                    | t' <- [1..t]]
--    | otherwise = sigma [
--                    sigma [
--                        sigma [
--                            prod [c graph k' t' j, c graph (k-k') (t-t') i]
--                            | (i,j) <- graph]
--                        | k' <- [1..k]]
--                    | t' <- [1..t]]
