fun fib 0 = 0
  | fib 1 = 1
  | fib i = fib (i-1) + fib (i-2)

fun main n = fib n
