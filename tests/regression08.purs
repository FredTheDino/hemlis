module T where

a =
  let
    -- A
    b = 1
  in
  1

a =
  let
    c =
      ( case 1 of
          true -> 1
          false -> 2
      )
  in
  1

-- + expected stdout:
-- + 40 of 40
-- + ===
-- + 
-- + ===
-- + 
-- + ===

