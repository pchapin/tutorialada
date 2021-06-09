---------------------------------------------------------------------------
-- FILE   : prime_factors.adb
-- SUBJECT: Finds all the prime factors of a given integer.
--
-- This program extends the functionality of prime.adb and prime2.adb by computing all the
-- prime factors of a given value and displaying them in a "nice" way. It illustrates command
-- line handling and gives further examples of control structures, etc.
--
-- Exercise: Modify this program so that if no command line argument is provided, it instead
-- reads its standard input device in a loop and factors every number it finds there (you can
-- assume each number is entered on separate lines).
---------------------------------------------------------------------------
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line;    use Ada.Command_Line;

procedure Prime_Factors is
   N : Positive;

   -- Finds the first (smallest) factor of N along with corresponding exponent.
   procedure Find_First_Factor
      (N : in out Positive; Factor : out Positive; Exponent : out Positive) is
   begin
      Factor   := 2;
      Exponent := 1;

      -- Find smallest factor.
      while (N rem Factor) /= 0 loop
         Factor := Factor + 1;
      end loop;

      -- Extract that factor...
      N := N / Factor;

      -- ... as many times as possible.
      while (N rem Factor) = 0 loop
         Exponent := Exponent + 1;
         N := N / Factor;
      end loop;
   end Find_First_Factor;


   procedure Find_Factors(N : in Positive) is
      Number     : Positive := N;  -- Must copy for use as 'out' parameter in call below.
      Factor     : Positive;
      Exponent   : Positive;
      Print_Star : Boolean := False;
   begin
      Put(Number, 0); Put(" = ");
      while Number /= 1 loop
         Find_First_Factor(Number, Factor, Exponent);

         -- Display this factor.
         if Print_Star then
            Put(" * ");
         end if;
         Print_Star := True;
         Put(Factor, 0);
         if Exponent > 1 then
            Put("^"); Put(Exponent, 0);
         end if;
      end loop;
      New_Line;
   end Find_Factors;

begin -- Prime_Factors

   -- The main program analyzes the command line and calls Find_Factors.
   if Argument_Count /= 1 then
      Put_Line("Usage: prime_factors n");
      Set_Exit_Status(Failure);
   else
      N := Integer'Value(Argument(1));
      if N = 1 then
         Put_Line("Invalid N. Use a value greater than one.");
         Set_Exit_Status(Failure);
      else
         Find_Factors(N);
      end if;
   end if;
   Set_Exit_Status(Success);
end Prime_Factors;
