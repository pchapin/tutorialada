---------------------------------------------------------------------------
-- FILE    : prime2.adb
-- SUBJECT : This program checks if a given number is prime.
--
-- Although this program uses the same basic method as that used by prime.adb, it factors the
-- core algorithm into a separate, nested function. Separating computation from I/O is good
-- practice in general since it makes the computation more reusable. Compare this code with
-- that in prime.adb.
--
-- Exercise: Implement some superior (faster) primality checking method.
---------------------------------------------------------------------------

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Prime2 is
   Number : Integer;

   -- This function returns True if N is prime; False otherwise.
   -- Note that it assumes N >= 2.
   function Is_Prime(N : Integer) return Boolean is
   begin
      for I in 2 .. (N - 1) loop
         if N mod I = 0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_Prime;

begin
   Put("Enter an integer: ");
   Get(Number);
   if Number < 2 then
      Put("The value "); Put(Number, 0); Put_Line(" is invalid.");
   else
      Put("The value "); Put(Number, 0);
      if Is_Prime(Number) then
         Put_Line(" is prime.");
      else
         Put_Line(" is not prime.");
      end if;
   end if;
end Prime2;
