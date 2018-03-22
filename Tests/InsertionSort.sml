fun foldl f s nil = s
  | foldl f s (x::xr) = foldl f (f(x,s)) xr

fun insert (x, nil) = [x]
  | insert (x, y::yr) = if x<=y then x::y::yr else y::insert(x,yr)
fun isort xs = foldl insert nil xs

fun head (x::_) = x

fun main i = head(isort([5, 4, 3, 2, 1]))
