---------------------------------------------------------------------------
-- FILE          : sieve_task.adb
-- LAST REVISION : 2008-06-22
-- SUBJECT       : Finds prime numbers using a background task.
-- PROGRAMMER    : (C) Copyright 2008 by Peter C. Chapin
--
-- This program is contrived, but it illustrates the basics of Ada's tasking support.
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin
--      Computer Information Systems
--      Vermont Technical College
--      Randolph Center, VT 05061
--      Peter.Chapin@vtc.vsc.edu
---------------------------------------------------------------------------
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line;    use Ada.Command_Line;

procedure Sieve_Task is

   type Flag_Array_Type is array(Positive range <>) of Boolean;
   type Flag_Access is access Flag_Array_Type;

   task Do_Sieve is
      entry Cancel;
      entry Provide_Bound(Bound : in Positive);
      entry Get_Result(Result : out Flag_Access);
   end Do_Sieve;

   task body Do_Sieve is
      Flags       : Flag_Access;
      J           : Integer;
   begin
      -- Wait here until someone gives us a bound or tells us to die.
      select
         accept Cancel do
            return;
         end Cancel;
      or
         accept Provide_Bound(Bound : in Positive) do
            Flags := new Flag_Array_Type(2 .. Bound);
         end Provide_Bound;
      end select;

      Flags.all := (others => True);

      -- Do the computations.
      for I in Flags'Range loop
         if Flags(I) = True then
            J := I + I;
            while J <= Flags'Last loop
               Flags(J) := False;
               J := J + I;
            end loop;
         end if;
      end loop;

      -- Wait here until someone asks for the result.
      accept Get_Result(Result : out Flag_Access) do
         Result := Flags;
      end Get_Result;
   end Do_Sieve;

   Upper_Bound : Integer;
   Answer      : Flag_Access;

begin -- Sieve_Task
   if Argument_Count /= 1 then
      Put_Line("Usage: sieve upper_bound");
      Do_Sieve.Cancel;
      Set_Exit_Status(Failure);
   else
      Upper_Bound := Integer'Value(Argument(1));
      if not (Upper_Bound > 2) then
         Put_Line("Invalid upper bound. Use a value greater than two.");
         Do_Sieve.Cancel;
         Set_Exit_Status(Failure);
      else
         Do_Sieve.Provide_Bound(Upper_Bound);
         Put_Line("Here I do things while Do_Sieve works out the primes.");
         Do_Sieve.Get_Result(Answer);
         for I in Answer'Range loop
            if Answer(I) = True then
               Put(I);
               New_Line;
            end if;
         end loop;

         -- Should deallocate Answer unless implementation collects garbage.
      end if;
   end if;
end Sieve_Task;

