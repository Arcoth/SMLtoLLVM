fun benchmark f n = let 
  fun aux i min = let
    val timer = Timer.startRealTimer()
    val _ = f()
    val time = Time.toMilliseconds(Timer.checkRealTimer(timer))
  in if time < min then aux 0 time else  aux (i-1) min end
in
  aux 0 1000000000 (* 11 days? *)
end
