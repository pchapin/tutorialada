---------------------------------------------------------------------------
-- FILE          : sieve.adb
-- SUBJECT       : Finds prime numbers.
--------------------------------------------------------------------------
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line;

procedure Sieve is
   Upper_Bound : Integer;

   -- Compute prime numbers between 2 and Upper_Bound; printing results.
   procedure Do_Sieve is
      Flags    : array(2 .. Upper_Bound) of Boolean := (others => True);
      Multiple : Integer;
   begin
      for I in Flags'Range loop
         if Flags(I) = True then
            Put(I); New_Line;
            Multiple := I + I;
            while Multiple <= Upper_Bound loop
               Flags(Multiple) := False;
               Multiple := Multiple + I;
            end loop;
         end if;
      end loop;
   end Do_Sieve;

begin -- Sieve

   -- The main program analyzes the command line and calls Do_Sieve.
   if Ada.Command_Line.Argument_Count /= 1 then
      Put_Line("Usage: sieve upper_bound");
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
   else
      Upper_Bound := Integer'Value(Ada.Command_Line.Argument(1));
      if not (Upper_Bound > 2) then
         Put_Line("Invalid upper bound. Use a value greater than two.");
         Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
      else
         Do_Sieve;
      end if;
   end if;
   Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Success);
end Sieve;
