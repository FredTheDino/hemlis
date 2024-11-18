a :: A -> { a :: A , a :: A } -> A a a A

--+ expected stdout:
--+ B
--+ 19 of 19
--+ ===
--+ Unexpected(Known(0, 1, 0), Some(Lower("a")), "T::Lower(\"module\")")
--+ >>>>>
--+ a :: A -> { a :: A , a :: A } -> A a a A
--+ <<<<<<
--+ ===

