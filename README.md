tmpla: tempolary language / compiler
===

習作コンパイラ

## Sample code

```
main := fact 5;

fact :: Int -> Int
fact n := let
    iszero n := n = 0;
    in if iszero n then 1
                   else n * fact (n - 1);
```

