with Ada.Numerics.Generic_Elementary_Functions;

package body Number_Theory is

   -- Instantiate the library for floating point math using Floating_Type.
   package Floating_Functions is new Ada.Numerics.Generic_Elementary_Functions(Floating_Type);
   use Floating_Functions;


   function Factorial(N : in Factorial_Argument_Type) return Positive is
   begin
      -- TODO: Finish me!
      --
      -- 0! is 1
      -- N! is N * (N-1) * (N-2) * ... * 1
      return 1;
   end Factorial;


   function Is_Prime(N : in Prime_Argument_Type) return Boolean is
      Upper_Bound     : Prime_Argument_Type;
      Current_Divisor : Prime_Argument_Type;
   begin
      -- Handle 2 as a special case.
      if N = 2 then
         return True;
      end if;

      Upper_Bound := N - 1;
      Current_Divisor := 2;
      while Current_Divisor < Upper_Bound loop
         if N rem Current_Divisor = 0 then
            return False;
         end if;
         Upper_Bound := N / Current_Divisor;
      end loop;
      return True;
   end Is_Prime;


   function Prime_Counting(N : in Prime_Argument_Type) return Natural is
   begin
      -- TODO: Finish me!
      --
      -- See the lab page for more information.
      return 0;
   end Prime_Counting;


   function Logarithmic_Integral(N : in Prime_Argument_Type) return Floating_Type is
   begin
      -- TODO: Finish me!
      --
      -- See the lab page for more information.
      return 1.0;
   end Logarithmic_Integral;

end Number_Theory;
