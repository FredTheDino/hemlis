module T where

a =
  case 1 of
    1 ->
      A.do
        1
    1 -> 1

--+ expected stdout:
--+ B
--+ 18 of 18
--+ ===
--+ Unexpected(Known(0, 1, 0), Some(Lower("a")), "T::Lower(\"module\")")
--+ >>>>>
--+ a = case 1 of
--+ <<<<<<
--+ ===
