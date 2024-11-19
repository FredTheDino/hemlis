module T where

a =
  let
    a =
      ado
        a
        in {}

    b = foo

  in
  1

--+ expected stdout:
--+ B
--+ 20 of 20
--+ ===
--+ Unexpected(Known(0, 1, 0), Some(Lower("a")), "T::Lower(\"module\")")
--+ >>>>>
--+ a =
--+ <<<<<<
--+ ===
