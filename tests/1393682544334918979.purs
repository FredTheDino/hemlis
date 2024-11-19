module T where
a = let 
    
  a = a >>> A.a a = A.a >>> a [ A , A ] >>> A.a a { a , a } = { a : A.a $ A.a a , a : A.a $ A.a a } a = A.a >>> a [ A ] >>> A.a A.A >>> A.a [ A ] >>> A.a a a a = { a : a a , a : a a } in A.a "a" [ A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) --- , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) ( a "a" ) # A.a # A.a # A.a ( a "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) ( a "a" ) # A.a # A.a # A.a ( a "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) ( a "a" ) # A.a # A.a # A.a ( a "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) ( a "a" ) # A.a # A.a # A.a ( a "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) # A.a # A.a # A.a ( a "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) # A.a # A.a # A.a ( a "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) # A.a # A.a # A.a ( a "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) # A.a # A.a # A.a ( a "a" ) ]

-- expected stdout:
-- C
-- 836 of 835
-- ===
-- Unexpected(Known(0, 1, 0), Some(Lower("a")), "T::Lower(\"module\")")
-- >>>>>
-- a = let 
-- <<<<<<
-- NotAtEOF(Known(1911, 1912, 0), None)
-- >>>>>
-- 
--   a = a >>> A.a a = A.a >>> a [ A , A ] >>> A.a a { a , a } = { a : A.a $ A.a a , a : A.a $ A.a a } a = A.a >>> a [ A ] >>> A.a A.A >>> A.a [ A ] >>> A.a a a a = { a : a a , a : a a } in A.a "a" [ A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) --- , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) A.A ( a "a" ) # a # A.a ( a "a" "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) ( a "a" ) # A.a # A.a # A.a ( a "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) ( a "a" ) # A.a # A.a # A.a ( a "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) ( a "a" ) # A.a # A.a # A.a ( a "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) ( a "a" ) # A.a # A.a # A.a ( a "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) # A.a # A.a # A.a ( a "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) # A.a # A.a # A.a ( a "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) # A.a # A.a # A.a ( a "a" ) , A.a "a" \ a -> A.a ( a "a" ) ( A A A ) # A.a # A.a # A.a ( a "a" ) ]
-- <<<<<<
-- ===

-- expected stderr:
-- [src/parser.rs:1765:9] self.peek_() = (
--     None,
--     Known(
--         1911,
--         1912,
--         0,
--     ),
-- )

--+ expected stdout:
--+ B
--+ 836 of 836
--+ ===
--+ Unexpected(Known(0, 1, 0), Some(Lower("a")), "T::Lower(\"module\")")
--+ >>>>>
--+ a = let 
--+ <<<<<<
--+ ===

