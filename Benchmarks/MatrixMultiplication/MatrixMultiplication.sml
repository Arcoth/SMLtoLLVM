(* Benchmark function *)

fun benchmark f n = let 
  fun aux i min = 
    if i = n 
    then min 
    else let
        val timer = Timer.startRealTimer()
        val _ = f()
        val time = Time.toMilliseconds(Timer.checkRealTimer(timer))
      in 
        if time < min 
        then aux 0 time 
        else aux (i+1) min 
      end
in
  aux 0 100000000
end

fun benchmarkAll (_, _, _, 0, _) = ()
  | benchmarkAll (function, start_arg, stride, num_strides, repeats) = 
  (print ((LargeInt.toString(benchmark (fn () => function start_arg) repeats)) ^ "\n");
   benchmarkAll (function, start_arg + stride, stride, num_strides - 1, repeats))

(* generation: *)
fun makeList n (s : real) = let
  fun aux (0, _, l) = l
    | aux (m, t, l) = aux(m-1, t+1.0, (t*t) :: l)
in aux(n, s, nil) end

fun makeListOf n s f = let
  fun aux (0, _, l) = l
    | aux (m, t, l) = aux(m-1, t+1.0, (f t) :: l)
in aux(n, s, nil) end

fun matrix n s = makeListOf n s (makeList n)

(* multiplication: *)
fun appendToAll(nil, nil) = nil
  | appendToAll(x::xs, l::ls) = (x::l) :: appendToAll(xs, ls)

fun transpose (l::ls) = let 
  fun listify nil = nil
    | listify (x::xs) = [x] :: listify(xs)
  fun appendRecursively nil r = r
    | appendRecursively (l::ls) r = appendRecursively ls (appendToAll(l, r))
  in 
    appendRecursively ls (listify l)
end

fun inner_product lhs rhs = let 
  fun aux (nil, nil, r) = r
    | aux (x::xs, y::ys, r) = aux (xs, ys, x*y+r)
in 
  aux (lhs, rhs, 0.0)
end

fun inner_products (_, nil) = nil
  | inner_products (vec, v::vectors) = let 
  val f = inner_product vec
in (f v) :: inner_products(vec, vectors) end

fun matrix_product a b = let
 fun prod (nil, _) = nil
   | prod (row_a::rows_a, rows_b) = let
   val i = inner_products(row_a, rows_b)
  in i :: prod(rows_a, rows_b) end
in 
  prod (a, transpose b)
end

(* Hash for verification (treat a list as a polynomial): *)
fun hash_list (x::xs) = x + 0.9 * hash_list xs
  | hash_list nil = 0.0

fun hash_matrix (x::xs) = hash_list x + 0.8 * hash_matrix xs
  | hash_matrix nil = 0.0

fun main() = benchmarkAll ((fn n => hash_matrix (matrix_product (matrix n 0.0) (matrix n 1.0))), 100, 50, 20, 5) 
