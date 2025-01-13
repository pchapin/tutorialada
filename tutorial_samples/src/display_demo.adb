with Ada.Text_IO;

-- This procedure is intended to demonstrate scalar types.
procedure Display_Demo is

   -- A package to manage a hypothetical 4x40 LCD display in an embedded system.
   -- In real life this package would more likely be at library level.
   package Display is

      type Row_Type is range 1 .. 4;
      type Column_Type is range 1 .. 40;

      -- Prints the given message at the specificed location.
      -- If the message is too long for the for the space available on the row, the message is truncated.
      procedure Print_At(Row : in Row_Type; Column : in Column_Type; Message : in String);

   end Display;


   package body Display is

      procedure Print_At(Row : in Row_Type; Column : in Column_Type; Message : in String) is
      begin
         -- In real life, this would communicate with the display.
         Ada.Text_IO.Put_Line("Row: " & Row_Type'Image(Row) & ", Col: " & Column_Type'Image(Column) & ", " & Message);
      end Print_At;

   end Display;

begin
   for Current_Row in Display.Row_Type range 2 .. 3 loop
      Display.Print_At(4, Current_Row, "Hello!");
   end loop;
end Display_Demo;
