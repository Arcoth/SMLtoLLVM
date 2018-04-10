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
  | benchmarkAll (function, start_arg, factor, num_strides, repeats) = 
  (print (Real.toString(Math.ln(Real.fromLargeInt(benchmark (fn () => function start_arg) repeats)) / Math.ln (real factor)) ^ ",\n");
   benchmarkAll (function, start_arg * factor, factor, num_strides - 1, repeats))


(**
 ** $Id: Complex.sml 89 2005-12-16 17:00:43Z rlpm $
 **
 ** A complex number library for Standard ML
 ** Author: Rory McGuire <rlpm at cs dot unm dot edu>
 **
 ** This program is free software; you can redistribute it and/or
 ** modify it under the terms of the GNU General Public License as
 ** published by the Free Software Foundation; either version 2 of the
 ** License, or (at your option) any later version.
 **
 ** This program is distributed in the hope that it will be useful,
 ** but WITHOUT ANY WARRANTY; without even the implied warranty of
 ** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 ** General Public License for more details.
 **
 ** You can obtain a copy of the GPL at:
 **   http://www.fsf.org/copyleft/gpl.html.
 **)

structure Complex =
struct 
  infix 6 + -
  infix 7 * / 

  type complex = {real:real, imag:real}

  val i = {real=0.0,imag=1.0}

  fun fromReal r =
      {real=r,imag=0.0}

  val fromInt = fromReal o Real.fromInt

  fun magnitude {real,imag} =
      Math.sqrt(Real.+(Real.*(real,real),Real.*(imag,imag)))

  fun phase {real,imag} =
      if Real.==(imag,0.0)
      then if real < 0.0
           then Math.pi
           else 0.0
      else if Real.==(real,0.0)
      then if imag < 0.0
           then Real.~(Real./(Math.pi,2.0))
           else Real./(Math.pi,2.0)
      else
          let
              val a = Math.atan (Real./(imag,real))
          in
              if real < 0.0
              then if imag < 0.0
                   then a - Math.pi
                   else a + Math.pi
              else a
          end	

  fun toPolar x =
      {magnitude=magnitude x, phase=phase x}

  fun fromPolar {magnitude,phase} =
      {real=Real.*(magnitude,Math.cos phase),
       imag=Real.*(magnitude,Math.sin phase)}

  fun toString {real,imag} =
      Real.toString real ^ "+" ^ Real.toString imag ^ "i"

  fun ~ {real,imag} =
      {real=Real.~(real),imag=Real.~(imag)}

  fun conj {real,imag} =
      {real=real,imag=Real.~(imag)}

  val ~~ = conj

  fun op+ ({real=r1,imag=i1},{real=r2,imag=i2}) =
      {real=Real.+(r1,r2),imag=Real.+(i1,i2)}
  
  fun op- ({real=r1,imag=i1},{real=r2,imag=i2}) =
      {real=Real.-(r1,r2),imag=Real.-(i1,i2)}

  fun op* ({real=x,imag=y},{real=a,imag=b}) =
      {real=Real.-(Real.*(x,a),Real.*(y,b)),
       imag=Real.+(Real.*(y,a),Real.*(x,b))}
  
  fun op/ ({real=x,imag=y},{real=a,imag=b}) =
      let
          fun f x = Real./(x,Real.+(Real.*(a,a),Real.*(b,b)))
      in
          {real=f (Real.+(Real.*(x,a),Real.*(y,b))),
           imag=f (Real.+(Real.*(y,a),Real.*(x,b)))}
      end
  

  fun exp {real,imag} =
      fromPolar{magnitude=Math.exp(real),phase=imag}
 
  fun ln x =
      let
          val {magnitude,phase} = toPolar x
      in
          {real=Math.ln magnitude,imag=phase}
      end
 
  fun pow (z, {real=x,imag=y}) =
      let
          val {magnitude=a,phase=p} = toPolar z
          val lna = Math.ln a
      in
          exp({real=Real.-(Real.*(x,lna),Real.*(p,y)),
               imag=Real.+(Real.*(p,x),Real.*(y,lna))})
      end

  fun sin {real=x,imag=y} =
      {real=Real.*(Math.sin x,Math.cosh y),
       imag=Real.*(Math.cos x,Math.sinh y)}

  fun cos {real=x,imag=y} =
      {real=Real.*(Math.cos x,Math.cosh y),
              imag=Real.~(Real.*(Math.sin x,Math.sinh y))}
end

fun exp_pi x = Complex.exp({real=0.0, imag = ~2.0 * Math.pi * x})

fun concat(x::xs, y) = x :: concat(xs, y)
  | concat(nil, y) = y

local structure C = Complex in

	fun fft x = let

		fun fft_aux(x::_, to_drop, 1, _) = [x]
		  | fft_aux(x : Complex.complex list, to_drop, N : int, s : int) = 
		let 
			val X1 = fft_aux(List.drop(x, to_drop), 0, N div 2, 2*s) 
			val X2 = fft_aux(x, to_drop + s, N div 2, 2*s)

			fun new_x1(x1::x1s, x2::x2s, k) = (C.+(x1, C.*(exp_pi(real(k)/real(N)), x2))) :: new_x1(x1s, x2s, k+1)
			  | new_x1(_, _, _) = nil

			fun new_x2(x1::x1s, x2::x2s, k) = (C.-(x1, C.*(exp_pi(real(k)/real(N)), x2))) :: new_x2(x1s, x2s, k+1)
			  | new_x2(_, _, _) = nil

			val first = new_x1(X1, X2, 0)
			val second = new_x2(X1, X2, 0)
		in
		  concat(first, second)
		end 

	in 
		fft_aux(x, 0, List.length x, 1)
	end

end

fun hash_complex{real, imag} = real + imag * 0.5


fun makeList n (s : real) = let
  fun aux (0, _, l) = l
    | aux (m, t, l) = let
      val base = t*t
      val adjusted = if base > 10.0 then base - 10.0 else base
     in
       aux(m-1, t+1.0, (Complex.fromReal adjusted) :: l)
     end
in aux(n, s, nil) end

(* Hash for verification (treat a list as a polynomial): *)
fun hash_list (x::xs) = hash_complex(x) + 0.9 * hash_list xs
  | hash_list nil = 0.0

fun main() = benchmarkAll((fn n => hash_list(fft(makeList n 0.0))), 4096, 2, 8, 5)
