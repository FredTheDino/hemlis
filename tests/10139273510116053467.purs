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

--+ expected stdout:
--+ B
--+ 38 of 38
--+ ===
--+ Unexpected(Known(0, 1, 0), Some(Lower("a")), "T::Lower(\"module\")")
--+ >>>>>
--+ a = let 
--+ <<<<<<
--+ ===
