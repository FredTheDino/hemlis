a = A.do 
        { a
        , a
        }

--+ expected stdout:
--+ B
--+ 11 of 11
--+ ===
--+ Unexpected(Known(0, 1, 0), Some(Lower("a")), "T::Lower(\"module\")")
--+ >>>>>
--+ a = A.do 
--+ <<<<<<
--+ ===

