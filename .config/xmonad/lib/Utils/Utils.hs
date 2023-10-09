module Utils.Utils
  ( join,
  )
where

join :: [String] -> String -> String
join [] _ = ""
join [x] _ = x
join (x : y : r) sep = x ++ sep ++ join (y : r) sep
