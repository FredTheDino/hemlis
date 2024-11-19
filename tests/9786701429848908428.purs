module T where

a =
  do
    A.a { a: "a", a }
    A.a

--+ expected stdout:
--+ B
--+ 16 of 16
--+ ===
--+ Unexpected(Known(0, 1, 0), Some(Lower("a")), "T::Lower(\"module\")")
--+ >>>>>
--+ a = do 
--+ <<<<<<
--+ ===
