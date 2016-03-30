with Ada.Text_IO; use Ada.Text_IO;

-- This is a contrived program designed to show off Ada's strong typing.
procedure Strong is
   
   type Row_Type is range 1..25;
   type Col_Type is range 1..80;

   package Row_IO is new Integer_IO(Row_Type); use Row_IO;
   package Col_IO is new Integer_IO(Col_Type); use Col_IO;

   procedure Put_Pixel(Row : in Row_Type; Col : in Col_Type) is
   begin
      Put("Putting pixel at (row = "); Put(Row); Put(", col = "); Put(Col); Put_Line(")");
   end Put_Pixel;

   Initial_Row : Row_Type := 12;
   Initial_Col : Col_Type := 40;

begin
   -- Compile error! Accidently switched arguments.
   Put_Pixel(Initial_Col, Initial_Row);

   -- Compile error! Illogical mixing of row and column information.
   Initial_Row := Initial_Row + Initial_Col;

   -- Use type conversion to force issue when necessary. Now documented.
   Initial_Row := Row_Type(Initial_Col)/2;

   -- Runtime error! Out of bounds result.
   Initial_Row := 10 * Initial_Row;
end Strong;

