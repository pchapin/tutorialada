---------------------------------------------------------------------------
-- FILE    : prime.adb
-- SUBJECT : This program checks if a given number is prime.
--
-- This program uses a very simplistic method for checking primality. Note that this program is
-- suitable as a main program since it is a parameterless library level (not nested in anything)
-- procedure.
--
-- Exercise: What happens if you enter something that is not a number? Can you fix it?
---------------------------------------------------------------------------

-- Used in the tutorial section on control structures...
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Prime is
   Number : Integer;
begin
   Put("Enter an integer: ");
   Get(Number);
   if Number < 2 then
      Put("The value "); Put(Number, 0); Put_Line(" is invalid.");
   else
      Put("The value "); Put(Number, 0);
      for I in 2 .. (Number - 1) loop
         if Number rem I = 0 then
            Put_Line(" is not prime.");
            return;
         end if;
      end loop;
      Put_Line(" is prime.");
   end if;
end Prime;

