module T where

a =
  let
    a = a
  in
  A.ado
    a <- A.a
    in
      A.a
        ( \a ->
            A.ado
              a <- A.a
              in A a
        )
        a

-- + expected stdout:
-- + 42 of 42
-- + ===
-- + 
-- + ===
-- + 
-- + ===

