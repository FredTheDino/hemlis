module T where

f :: F
f =
  a.b
    <#> (\x -> x)

-- + expected stdout:
-- + 21 of 21
-- + ===
-- + 
-- + ===
-- + 
-- + ===

