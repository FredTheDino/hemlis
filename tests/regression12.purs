module T where
a ps =
  case unit of
    -- A
    _ | A.a -> A
    -- B
    _ -> 1
    -- D
    _ -> D

-- + expected stdout:
-- + 27 of 27
-- + ===
-- + 
-- + ===
-- + 
-- + ===

