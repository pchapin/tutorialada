with Ada.Numerics.Generic_Elementary_Functions;

package body Number_Theory is

   -- Instantiate the library for floating point math using Floating_Type.
   package Floating_Functions is new Ada.Numerics.Generic_Elementary_Functions(Floating_Type);
   use Floating_Functions;


   function Factorial(N : in Factorial_Argument_Type) return Positive is
      Result : Positive := 1;
   begin
      if N > 0 then
         for I in 1 .. N loop
            Result := Result * I;
         end loop;
      end if;
      return Result;
   end Factorial;


   function Is_Prime(N : in Prime_Argument_Type) return Boolean is
      Upper_Bound     : Positive;
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
         Current_Divisor := Current_Divisor + 1;
      end loop;
      return True;
   end Is_Prime;


   function Prime_Counting(N : in Prime_Argument_Type) return Natural is
      Count : Natural := 0;
   begin
      for I in 2 .. N loop
         if Is_Prime(I) then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Prime_Counting;


   function Logarithmic_Integral(N : in Prime_Argument_Type) return Floating_Type is
      Result : Floating_Type;
      Term   : Floating_Type;
   begin
      Result := Gamma + Log(Log(Floating_Type(N)));
      -- How many terms should we compute? The 11th term is the last that won't overflow
      -- the multipliciation I * Factorial(I). However, to get a good estimate of li(N)
      -- we need considerably more terms.
      --
      --for I in 1 .. 11 loop
      --   Term := (Log(Floating_Type(N)) ** I) / Floating_Type((I * Factorial(I)));
      --   Result := Result + Term;
      --end loop;
      --
      -- The approach is to edit Term for each iteration.
      Term   := Log(Floating_Type(N)); -- Compute term #1 as a special case.
      Result := Result + Term;         -- ... and add it into the result.
      for I in 2 .. 1000 loop            -- Compute all other terms based on previous term.
         Term := Term * Log(Floating_Type(N)) * (Floating_Type(I - 1)/Floating_Type(I*I));
         Result := Result + Term;
      end loop;
      return Result;
   end Logarithmic_Integral;

end Number_Theory;
