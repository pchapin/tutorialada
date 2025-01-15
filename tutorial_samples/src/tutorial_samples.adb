---------------------------------------------------------------------------
-- FILE    : tutorial_samples.adb
-- SUBJECT : A simple menu-driven program to exercise the samples in the tutorial.
--
---------------------------------------------------------------------------
with Ada.Text_IO;
with Hello;
with Samples_Test;

procedure Tutorial_Samples is
   use Ada.Text_IO;
begin
   Put_Line("This program demonstrates the various sample programs in the tutorial.");
   New_Line;

   Process_Selections :
   loop
      -- Display the menu.
      Put_Line("0. Exit");
      Put_Line("1. Hello");
      Put_Line("2. Unit Tests");

      Put("Selection: ");
      declare
         Command : constant String := Get_Line;
      begin
         New_Line;

         -- Here we assume there is a first character in the line. If that is not true (i.e., a
         -- blank line is entered), a Constraint_Error exception will be raised. As an
         -- exercise, add error handling to this code.
         case Command(1) is
            when '0' => exit Process_Selections;
            when '1' => Hello;
            when '2' => Samples_Test;
            when others =>
               Put_Line("Unknown selection: " & Command);
         end case;
         New_Line;
      end;
   end loop Process_Selections;
end Tutorial_Samples;
