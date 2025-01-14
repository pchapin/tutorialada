with Ada.Characters.Latin_1;
with Ada.Text_IO;
--  with Ada.Integer_Text_IO;
with Interfaces.C;
with C_Library;

use Interfaces.C;

procedure Interfacing_Sample is
   A, B   : Integer;
   C      : int;
   Holder : C_Library.IntFormat;
begin
   A := 1;
   B := 2;
   C := C_Library.Sum (int (A), int (B));
   Ada.Text_IO.Put ("The sum is: ");
   C_Library.Print_Int (C);
   --  Ada.Integer_Text_IO.Put (Integer (C), 0);
   --  Ada.Text_IO.New_Line;

   Holder.Number := 42;
   Holder.Formatted := (others => To_C (Ada.Characters.Latin_1.NUL));
   C_Library.Format_Integer (Holder);
   Ada.Text_IO.Put_Line (To_Ada (Holder.Formatted));
end Interfacing_Sample;
