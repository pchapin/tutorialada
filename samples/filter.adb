with Ada.Text_IO; use Ada.Text_IO;

procedure Filter is
   -- Declare local variables and subprograms here.
begin
   -- Read the entire input one line at a time...
   while not End_Of_File(Standard_Input) loop
      declare
         Line : String := Get_Line;  -- Function Ada.Text_IO.Get_Line gets a complete line.
      begin
         Put_Line(Line);
      end;
   end loop;
end Filter;
