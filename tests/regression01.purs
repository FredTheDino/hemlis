module T where

a =
  case x of
    foo
      | true -> 1
      | false -> 2

-- + expected stdout:
-- + 21 of 21
-- + ===
-- + 
-- + ===
-- + 
-- + ===

