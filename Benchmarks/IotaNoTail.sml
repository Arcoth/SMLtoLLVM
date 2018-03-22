fun nth ((x::xs), i) = if i = 0 then x else nth(xs, i-1)

fun iota 0 = [0]
  | iota n = n :: iota(n-1)

fun main() = let 
  val n = 10000000
  val timer = Timer.startRealTimer()
  val x = nth(iota n, n-2)
 in print (LargeInt.toString(Time.toMilliseconds(Timer.checkRealTimer(timer)))) end

val a = main()
