a a { a , a , a } = A.do 
  
  a a { a , a , a : A } { a , a , a , a : A }

--+ expected stdout:
--+ B
--+ 37 of 37
--+ ===
--+ Unexpected(Known(0, 1, 0), Some(Lower("a")), "T::Lower(\"module\")")
--+ >>>>>
--+ a a { a , a , a } = A.do 
--+ <<<<<<
--+ ===

