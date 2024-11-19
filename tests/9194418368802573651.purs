module T where

a = A { a } a :: { a :: A } -> A

--+ expected stdout:
--+ B
--+ 16 of 16
--+ ===
--+ Unexpected(Known(0, 1, 0), Some(Lower("a")), "T::Lower(\"module\")")
--+ >>>>>
--+ a = A { a } a :: { a :: A } -> A
--+ <<<<<<
--+ ===
