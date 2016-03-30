---------------------------------------------------------------------------
-- FILE    : number_theory.ads
-- SUBJECT : Package providing various number theoretic services.
-- AUTHOR  : (C) Copyright 2015 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

package Number_Theory is

   -- Prime Numbers
   ----------------

   subtype Prime_Argument_Type is Integer range 2 .. Integer'Last;

   -- Return True if N is prime and False otherwise.
   function Is_Prime(N : Prime_Argument_Type) return Boolean;

   -- Return True if N is prime and False otherwise. This function is more efficient.
   function Is_Prime_Fast(N : Prime_Argument_Type) return Boolean;


   -- Greatest Common Divisor
   --------------------------

   -- Returns the greatest common divisor of X and Y.
   -- Notice that setting X and Y to type Positive is not the same as the precondition here.
   -- The precondition allows either X or Y to be zero, just not both of them.
   --
   function Greatest_Common_Divisor(X : Natural; Y : Natural) return Natural
     with Pre => not (X = 0 and Y = 0);


   -- NOTE
   -- Proving freedom from overflow in Factorial and Fibonacci below is tricky because the
   -- parameter upper bounds of 12 (for Factorial) and 46 (for Fibonacci) are "magic." Actually
   -- they arise because of the behavior of the two functions but explaining to the SPARK tools
   -- their significance is difficult. This might be a place where an external axiomatization or
   -- an Assume pragma would be appropriate. See the notes below.

   -- Factorial
   ------------

   subtype Factorial_Argument_Type is Integer range 0 .. 12;

   -- The value of Fac is approximated by Stirling's formula. This could be used, in theory, to
   -- put an upper bound on Fac that could be proved here (by induction) and then used to prove
   -- freedom from overflow in the body of Factorial. See the handling of Fib and Fibonacci
   -- below for a more detailed sketch.
   --
   function Fac(N : Factorial_Argument_Type) return Positive is
      (case N is
          when 0 => 1,
          when others => N * Fac(N - 1))
     with Ghost;
       -- Also need a postcondition that bounds Fac appropriately.

   -- Returns N!
   function Factorial(N : Factorial_Argument_Type) return Positive
     with Post => Factorial'Result = Fac(N);


   -- Fibonacci Numbers
   --------------------

   subtype Fibonacci_Argument_Type is Integer range 0 .. 46;

   -- It can be shown that the value returned by Fib function is the integer closest to
   -- \frac{\phi^n}{\sqrt{5}} where \phi is the Golden Ratio given by (1 + \sqrt{5})/2. See:
   -- http://www-personal.umich.edu/~copyrght/image/books/Spatial%20Synthesis2/s01math55fibo.pdf
   -- Thus we should be able to assert that Fib'Result < (1.6181**N)/2.2360 + 1.0. This will be
   -- needed to prove freedom from overflow in function Fibonacci when Old + Temp is computed.
   --
   function Fib(N : Fibonacci_Argument_Type) return Natural is
      (case N is
          when 0 | 1  => N,
          when others => Fib(N - 1) + Fib(N - 2))
     with
       Ghost,
       Post => Float(Fib'Result) < (1.6181**N)/2.2360 + 1.0;

   -- Returns the Nth Fibonacci number.
   function Fibonacci(N : Fibonacci_Argument_Type) return Natural
     with Post => Fibonacci'Result = Fib(N);

   function Fibonacci_Fast(N : Fibonacci_Argument_Type) return Natural
     with Post => Fibonacci_Fast'Result = Fib(N);

   -- Integer Square Root
   ----------------------

   subtype Square_Root_Domain is Integer range 0 .. 1_000_000;
   subtype Square_Root_Range  is Square_Root_Domain range 0 .. 1_000;

   function Square_Root(N : Square_Root_Domain) return Square_Root_Range
     with
       Post => (Square_Root'Result    ) * (Square_Root'Result    ) <= N and
               (Square_Root'Result + 1) * (Square_Root'Result + 1) > N;

end Number_Theory;
