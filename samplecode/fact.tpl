main :: Int
main := fact 5;

fact :: Int -> Int
fact n := let
    iszero :: Int -> Bool
    iszero n := n = 0;
    in if iszero n
        then 1 
        else n * fact (n-1);
