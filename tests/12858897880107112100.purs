module T where

a (A { a: a }) = 1

--+ expected stdout:
--+ B
--+ 12 of 12
--+ ===
--+ Unexpected(Known(0, 1, 0), Some(Lower("a")), "T::Lower(\"module\")")
--+ >>>>>
--+ a (A { a: a }) = 1
--+ <<<<<<
--+ ===
