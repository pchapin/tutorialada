---------------------------------------------------------------------------
-- FILE   : filter.adb
-- SUBJECT: A skeleton of a basic text filter.
--
-- It is sometimes convenient to create a program that reads lines of text from the standard
-- input device, processes them, and then writes the resulting processed material to the
-- standard output device. This program is a skeleton of such a "filter." Flesh it out for
-- whatever application is needed by expanding the processing done inside the while loop.
---------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Integer_Text_IO;

use Ada.Text_IO;
use Ada.Integer_Text_IO;

procedure Filter is
   -- Declare local variables and subprograms here.
   Line_Number : Positive := 1;
begin
   -- Read the entire input one line at a time...
   while not End_Of_File(Standard_Input) loop
      declare
         -- Function Ada.Text_IO.Get_Line gets a complete line of any length.
         -- Normally strings need to be sized when they are declared. Here the size is taken
         -- from the value returned by Get_Line, so we don't need to know the size ahead of time.
         -- However, it is necessary to declare Line here, which is why a new 'declare' block
         -- was created.
         Line : constant String := Get_Line;
      begin
         -- Include whatever processing of Line is desired.
         Put(Line_Number); Put(": "); Put_Line(Line);
         Line_Number := Line_Number + 1;
      end;
   end loop;
end Filter;
