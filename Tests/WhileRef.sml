
fun log (b, n) =
    let
        val counter = ref b
        val value = ref 0
    in
        while !counter <= n do
        ( counter := !(ref (!counter*b));
          value := !value + 1); 
 	!value
    end

fun main i = log(2, 50000)
