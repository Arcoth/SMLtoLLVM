structure Array = struct 
  fun vector a = Array.vector a

  fun update (a, i, x) = Array.update(a, i, x)

  fun tabulate (n, f) = let
    val a = Array.array(n, f 0)
    fun aux i = if i = n then () else (update(a, i, f i); aux (i+1))
  in
    aux 0; a
  end
end

structure Vector = struct
  fun tabulate (i, f) = Array.vector(Array.tabulate (i, f))
end

fun main n =
 let
    val v = Vector.tabulate (1, fn i => (i, i + 1))
 in
    v
 end
