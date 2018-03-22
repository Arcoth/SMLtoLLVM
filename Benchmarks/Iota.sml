fun nth ((x::xs), i) = if i = 0 then x else nth(xs, i-1)

fun iota n = let
  fun f (0, l) = l
    | f (n, l) = f (n-1, n :: l)
in f (n, nil) end

fun main() = let val n = 10000000 in nth(iota n, n-3) end
