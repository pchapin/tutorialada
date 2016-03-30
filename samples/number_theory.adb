---------------------------------------------------------------------------
-- FILE    : number_theory.adb
-- SUBJECT : Package providing various number theoretic services.
-- AUTHOR  : (C) Copyright 2015 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

package body Number_Theory is

   -- This function runs in \Theta(N) time.
   function Is_Prime(N : Prime_Argument_Type) return Boolean is
      Result : Boolean := True;
   begin
      for Trial_Divisor in Integer range 2 .. N - 1 loop
         if N mod Trial_Divisor = 0 then
            Result := False;
            exit;
         end if;
      end loop;
      return Result;
   end Is_Prime;


   -- This function runs in \Theta(\sqrt(N)) time.
   function Is_Prime_Fast(N : Prime_Argument_Type) return Boolean is
      Result : Boolean := True;
      Trial_Divisor : Prime_Argument_Type;
      Upper_Bound : Positive;
   begin
      Trial_Divisor := 2;
      Upper_Bound := N - 1;
      while Trial_Divisor <= Upper_Bound loop
         pragma Loop_Invariant(Trial_Divisor <= N);

         if N mod Trial_Divisor = 0 then
            Result := False;
            exit;
         end if;
         Upper_Bound := N / Trial_Divisor;
         Trial_Divisor := Trial_Divisor + 1;
      end loop;
      return Result;
   end Is_Prime_Fast;


   function Greatest_Common_Divisor(X : Natural; Y : Natural) return Natural is
      Remainder : Natural;
      A         : Natural;
      B         : Natural;
   begin
      A := X;
      B := Y;

      while B > 0 loop
         pragma Loop_Invariant(A >= 0 and B > 0);
         Remainder := A mod B;
         A := B;
         B := Remainder;
      end loop;
      return A;
   end Greatest_Common_Divisor;


   function Factorial(N : Factorial_Argument_Type) return Positive is
      Result : Positive := 1;
   begin
      for I in Factorial_Argument_Type range 1 .. N loop
         pragma Loop_Invariant(Result = Fac(I - 1));
         Result := Result * I;
      end loop;
      pragma Assert(Result = Fac(N));  -- It's a little weird that this is required.
      return Result;
   end Factorial;


   function Fibonacci(N : Fibonacci_Argument_Type) return Natural is
      Result : Natural;
      Old    : Positive;
      Oldest : Natural;
      Temp   : Natural;
   begin
      case N is
         when 0 | 1 =>
            Result := N;

         when others =>
            Oldest := 0;
            Old    := 1;
            for I in Fibonacci_Argument_Type range 2 .. N loop
               pragma Loop_Invariant(Old = Fib(I - 1) and Oldest = Fib(I - 2));

               Temp   := Oldest;
               Oldest := Old;
               Old    := Old + Temp;
            end loop;
            Result := Old;
      end case;
      return Result;
   end Fibonacci;


   function Fibonacci_Fast(N : Fibonacci_Argument_Type) return Natural is
      Lookup_Table : constant array(Fibonacci_Argument_Type) of Natural :=
        ( 0 =>           0,  1 =>             1,  2 =>             1,  3 =>           2,
          4 =>           3,  5 =>             5,  6 =>             8,  7 =>          13,
          8 =>          21,  9 =>            34, 10 =>            55, 11 =>          89,
         12 =>         144, 13 =>           233, 14 =>           377, 15 =>         610,
         16 =>         987, 17 =>         1_597, 18 =>         2_584, 19 =>       4_181,
         20 =>       6_765, 21 =>        10_946, 22 =>        17_711, 23 =>      28_657,
         24 =>      46_368, 25 =>        75_025, 26 =>       121_393, 27 =>     196_418,
         28 =>     317_811, 29 =>       514_229, 30 =>       832_040, 31 =>   1_346_269,
         32 =>   2_178_309, 33 =>     3_524_578, 34 =>     5_702_887, 35 =>   9_227_465,
         36 =>  14_930_352, 37 =>    24_157_817, 38 =>    39_088_169, 39 =>  63_245_986,
         40 => 102_334_155, 41 =>   165_580_141, 42 =>   267_914_296, 43 => 433_494_437,
         44 => 701_408_733, 45 => 1_134_903_170, 46 => 1_836_311_903
        );
   begin
      return Lookup_Table(N);
   end Fibonacci_Fast;


   function Square_Root(N : Square_Root_Domain) return Square_Root_Range is
      Q : Square_Root_Domain;
      R : Square_Root_Domain;
   begin
      Q := N;

      if N > 0 then
         loop
            R := N/Q;
            pragma Loop_Variant(Decreases => Q);
            pragma Loop_Invariant(R >= 1 and Q <= N and Q*R <= N);
            exit when Q <= R;
            Q := (Q + R)/2;
         end loop;
      end if;
      return Q;
   end Square_Root;

end Number_Theory;
