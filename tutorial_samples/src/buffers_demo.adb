---------------------------------------------------------------------------
-- FILE    : buffers_demo.ads
-- SUBJECT : A short program demonstrating the Buffers packages.
--
-- Feel free to edit this to try out various things!
---------------------------------------------------------------------------

with Ada.Text_IO;
-- with Ada.Integer_Text_IO;  -- Useful for printing formatted integers.

with Buffers1;

procedure Buffers_Demo is
   package Buffers renames Buffers1;

   My_Buffer : Buffers.Buffer_Type;
   Extracted_Text : String(1 .. 8);       -- An eight character string.
begin
   Buffers.Copy_Into(My_Buffer, "Hello, World!");
   Buffers.Copy_From
     (Buffer      => My_Buffer,
      Destination => Extracted_Text,
      Point       => 2,
      Length      => 4);

   Ada.Text_IO.Put_Line("|" & Extracted_Text & "|");
end Buffers_Demo;
