fun nth ((x::xs), i) = if i = 0 then x else nth(xs, i-1)

fun iota 0 = [0]
  | iota n = n :: iota(n-1)

fun main() = let 
  val n = 10000000
 in nth(iota n, n-2) end
