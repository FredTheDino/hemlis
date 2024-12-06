module T where

a =
  case 1 of
    1 ->
      A.do
        1
    1 -> 1

-- + expected stdout:
-- + 23 of 23
-- + ===
-- + 
-- + ===
-- + 
-- + ===

