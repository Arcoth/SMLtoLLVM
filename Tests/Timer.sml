fun main () =
  let 
    val timer = Timer.startRealTimer()
  in
      Time.toMilliseconds(Timer.checkRealTimer(timer))
  end
