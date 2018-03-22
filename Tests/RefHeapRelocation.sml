fun main() = let
  val x = ref 0
  val i = ref 0
in 
  (while !i < 2000000
   do let val new = ref 2 
   in x := !x + !new; i := !i + !new end
  ); !x
end
