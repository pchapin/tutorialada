---------------------------------------------------------------------------
-- FILE    : check_number_theory.adb
-- SUBJECT : Package containing tests of package Number_Theory.
-- AUTHOR  : (C) Copyright 2015 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with AUnit.Assertions; use AUnit.Assertions;
with Number_Theory;

use Number_Theory;

package body Check_Number_Theory is

   -------------------------
   -- Is_Prime and Is_Prime2
   -------------------------

   type Prime_Record is
      record
         N     : Prime_Argument_Type;
         Prime : Boolean;
      end record;

   -- Test cases for Is_Prime and Is_Prime_Fast.
   Prime_Cases : constant array(1 .. 10) of Prime_Record :=
     ((N =>             2, Prime => True ),  -- The first prime number.
      (N =>             3, Prime => True ),  -- The first odd prime number.
      (N =>             4, Prime => False),  -- The first number > 1 that is not prime.
      (N =>             5, Prime => True ),  -- The first prime number after a non-prime.
      (N =>             9, Prime => False),  -- The first non-prime odd number.
      (N =>           106, Prime => False),  --  2 * 53. Two factors; first smallest even prime.
      (N =>           111, Prime => False),  --  3 * 37. Two factors; first smallest odd prime.
      (N =>           961, Prime => False),  -- 31 * 31. The square of a prime.
      (N =>           953, Prime => True ),  -- A prime about the same as 961 above.
      (N => 2_147_483_647, Prime => True )   -- 2**31 - 1. Integer'Last on 32 bit systems.
     );

   procedure Test_Is_Prime(T : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      -- The last test case takes Is_Prime a long time to execute. Skip it for now.
      for I in Prime_Cases'First .. Prime_Cases'Last - 1 loop
         Assert(Is_Prime(Prime_Cases(I).N) = Prime_Cases(I).Prime,
                "Is_Prime(N) fails for N = " & Integer'Image(Prime_Cases(I).N));
      end loop;
   end Test_Is_Prime;

   procedure Test_Is_Prime_Fast(T : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      for I in Prime_Cases'Range loop
         Assert(Is_Prime_Fast(Prime_Cases(I).N) = Prime_Cases(I).Prime,
                "Is_Prime_Fast(N) fails for N = " & Integer'Image(Prime_Cases(I).N));
      end loop;
   end Test_Is_Prime_Fast;


   ------
   -- GCD
   ------

   type GCD_Record is
      record
         X        : Natural;
         Y        : Natural;
         Expected : Natural;
      end record;

   GCD_Cases : constant array(1 .. 6) of GCD_Record :=
     ((X =>      0, Y =>      2, Expected =>  2),
      (X =>      2, Y =>      0, Expected =>  2),
      (X => 13_020, Y =>  5_797, Expected => 31),   -- Not relatively prime.
      (X =>  5_797, Y => 13_020, Expected => 31),   -- Same as above in opposite order.
      (X =>    512, Y =>    135, Expected =>  1),   -- Relatively prime.
      (X =>    135, Y =>    512, Expected =>  1)    -- Same as above in opposite order.
     );

   procedure Test_Greatest_Common_Divisor(T : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      for I in GCD_Cases'Range loop
         Assert
           (Greatest_Common_Divisor(GCD_Cases(I).X, GCD_Cases(I).Y) = GCD_Cases(I).Expected,
            "Greatest_Common_Divisor(X, Y) fails for X = " & Integer'Image(GCD_Cases(I).X) & ", Y = " & Integer'Image(GCD_Cases(I).Y));
      end loop;
   end Test_Greatest_Common_Divisor;


   ------------
   -- Fibonacci
   ------------

   type Fibonacci_Record is
      record
         N        : Fibonacci_Argument_Type;
         Expected : Natural;
      end record;

   Fibonacci_Cases : constant array(1 .. 7) of Fibonacci_Record :=
     ((N =>  0, Expected =>             0),
      (N =>  1, Expected =>             1),
      (N =>  2, Expected =>             1),
      (N =>  3, Expected =>             2),
      (N =>  4, Expected =>             3),
      (N =>  7, Expected =>            13),
      (N => 19, Expected =>         4_181)
      --(N => 46, Expected => 1_836_311_903)  -- This is very slow to handle with assertions on.
     );

   procedure Test_Fibonacci(T : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      for I in Fibonacci_Cases'Range loop
         Assert
           (Fibonacci(Fibonacci_Cases(I).N) = Fibonacci_Cases(I).Expected,
            "Fibonacci(N) fails for N = " & Integer'Image(Fibonacci_Cases(I).N));
      end loop;
   end Test_Fibonacci;


   --------------
   -- Square_Root
   --------------

   type Square_Root_Record is
      record
         N        : Square_Root_Domain;
         Expected : Square_Root_Range;
      end record;

   Square_Root_Cases : constant array(1 .. 7) of Square_Root_Record :=
     ((N =>         0, Expected =>     0),
      (N =>         1, Expected =>     1),
      (N =>         2, Expected =>     1),
      (N =>         3, Expected =>     1),
      (N =>         4, Expected =>     2),
      (N =>         5, Expected =>     2),
      (N => 1_000_000, Expected => 1_000)
     );

   procedure Test_Square_Root(T : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      for I in Square_Root_Cases'Range loop
         Assert
           (Square_Root(Square_Root_Cases(I).N) = Square_Root_Cases(I).Expected,
            "Square_Root(N) fails for N = " & Integer'Image(Square_Root_Cases(I).N));
      end loop;
   end Test_Square_Root;


   procedure Register_Tests(T : in out Prime_Test) is
   begin
      Registration.Register_Routine(T, Test_Is_Prime'Access, "Is_Prime");
      Registration.Register_Routine(T, Test_Is_Prime_Fast'Access, "Is_Prime_Fast");
      Registration.Register_Routine(T, Test_Greatest_Common_Divisor'Access, "Greatest_Common_Divisor");
      Registration.Register_Routine(T, Test_Fibonacci'Access, "Fibonacci");
      Registration.Register_Routine(T, Test_Square_Root'Access, "Square_Root");
   end Register_Tests;


   function Name(T : Prime_Test) return AUnit.Message_String is
   begin
      return AUnit.Format("Number Theory");
   end Name;

end Check_Number_Theory;
