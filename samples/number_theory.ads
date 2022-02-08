-- This package contains subprograms for doing number theoretic computations.
-- It is used in VTC's CIS-2730, Lab #2.

package Number_Theory is

   -- Constant Definitions
   -----------------------
   Gamma : constant := 0.57721_56649_01532_86060_65120_90082_40243_10421_59335_93992;


   -- Type Definitions
   -------------------

   -- The range of values for which N! can be computed without overflow.
   subtype Factorial_Argument_Type is Integer range 0 .. 12;

   -- The range of values that might meaningfully be asked: are you prime?
   subtype Prime_Argument_Type is Integer range 2 .. Integer'Last;

   -- A floating point type with at least 15 significant decimal digits.
   type Floating_Type is digits 15;


   -- Subprogram Declarations
   --------------------------

   -- Returns N!
   function Factorial(N : Factorial_Argument_Type) return Positive;

   -- Returns True if N is prime; False otherwise.
   function Is_Prime(N : in Prime_Argument_Type) return Boolean;

   -- Returns the number of prime numbers less than or equal to N.
   function Prime_Counting(N : in Prime_Argument_Type) return Natural;

   -- The logarithmic integral function, which is an approximation of the prime counting function.
   function Logarithmic_Integral(N : in Prime_Argument_Type) return Floating_Type
     with Post => Logarithmic_Integral'Result > Floating_Type(Prime_Counting(N));

end Number_Theory;
