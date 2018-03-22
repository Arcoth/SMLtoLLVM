(* Written by Stephen Weeks (sweeks@sweeks.com). *)
fun merge (l1: int list, l2) =
   case (l1, l2) of
      ([], _) => l2
    | (_, []) => l1
    | (x1 :: l1', x2 :: l2') =>
         if x1 <= x2
            then x1 :: merge (l1', l2)
         else x2 :: merge (l1, l2')
      
fun tabulate(len, f) = let 
  fun aux(0, f, l) = (f 0) :: l
    | aux(n, f, l) = aux(n-1, f, (f n) :: l)
in 
  aux(len, f, nil) 
end

fun nth ((x::xs), i) = if i = 0 then x else nth(xs, i-1)


fun main size =
 let
    val len = 10000

    val l1 = tabulate (len, fn i => i * 2)
    val l2 = tabulate (len, fn i => i * 2 + 1)

    fun test () =
        10 = nth (merge (l1, l2), 10)

    fun loop n =
       if n < 2
          then test()
       else (test (); loop (n - 1))
 in
    loop size
 end

(*val _ = let
  val timer = Timer.startRealTimer() 
  val x = main 10000
in print (LargeInt.toString(Time.toMilliseconds(Timer.checkRealTimer(timer)))) end*)
