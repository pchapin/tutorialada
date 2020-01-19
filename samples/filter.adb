---------------------------------------------------------------------------
-- FILE   : filter.adb
-- SUBJECT: A skeleton of a basic text filter.
--
-- It is sometimes convenient to create a program that reads lines of text from the standard
-- input device, processes them, and then writes the resulting processed material to the
-- standard output device. This program is a skeleton of such a "filter." Flesh it out for
-- whatever application is needed by expanding the processing done inside the while loop.
---------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

procedure Filter is
   -- Declare local variables and subprograms here.
begin
   -- Read the entire input one line at a time...
   while not End_Of_File(Standard_Input) loop
      declare
         Line : String := Get_Line;  -- Function Ada.Text_IO.Get_Line gets a complete line.
      begin
         -- Include whatever processing of Line is desired.
         Put_Line(Line);
      end;
   end loop;
end Filter;
