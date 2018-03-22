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
  | inner_products (vec, v::vectors) = (inner_product vec v) :: inner_products(vec, vectors)

fun matrix_product a b = let
 fun prod (nil, _) = nil
   | prod (row_a::rows_a, rows_b) = inner_products(row_a, rows_b) :: prod(rows_a, rows_b)
in 
  prod (a, transpose b)
end

(* Hash for verification (treat a list as a polynomial): *)
fun hash_list (x::xs) = x + 0.9 * hash_list xs
  | hash_list nil = 0.0

fun hash_matrix (x::xs) = hash_list x + 0.8 * hash_matrix xs
  | hash_matrix nil = 0.0

fun main() = let 
  val timer = Timer.startRealTimer()
  val a = matrix 500 0.0
  val b = matrix 500 0.0
  val ab = hash_matrix (matrix_product a b)
  val time = Time.toMilliseconds(Timer.checkRealTimer(timer))
in ab end
