module T where

a =
  do
    A.a { a: "a", a }
    A.a

-- + expected stdout:
-- + 22 of 22
-- + ===
-- + 
-- + ===
-- + 
-- + ===

