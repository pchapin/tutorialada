---------------------------------------------------------------------------
-- FILE    : huffman.adb
-- SUBJECT : Main procedure of the Huffman encoding program.
-- AUTHOR  : (C) Copyright 2015 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <chapinp.acm.org>
---------------------------------------------------------------------------

-- Standard
with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Interfaces;

-- Application specific
with Code_Tree;

procedure Huffman is

   -- The following procedure displays byte count information, assigned codes, etc.
   procedure Print_Frequency_Table is
      package Byte_IO is new Ada.Text_IO.Modular_IO(Interfaces.Unsigned_8);
      package Fraction_IO is new Ada.Text_IO.Fixed_IO(Code_Tree.Fraction_Type);

      Byte_Data : Code_Tree.Byte_Information;
   begin
      -- Print table headers.
      Ada.Text_IO.Put_Line("      Byte       Count  Fraction");
      Ada.Text_IO.Put_Line("==========  ==========  ========");

      -- Print table.
      for Byte in Interfaces.Unsigned_8 loop
         Byte_Data := Code_Tree.Get_Byte(Byte);

         -- Print the byte.
         Byte_IO.Put(Item => Byte_Data.Value, Width => 6, Base => 16);
         if Ada.Characters.Handling.Is_Graphic(Character'Val(Byte_Data.Value)) then
            Ada.Text_IO.Put(" (");
            Ada.Text_IO.Put(Character'Val(Byte_Data.Value));
            Ada.Text_IO.Put(")  ");
         else
            Ada.Text_IO.Put("      ");
         end if;

         -- Print the count.
         Ada.Integer_Text_IO.Put(Item => Byte_Data.Count, Width => 10);
         Ada.Text_IO.Put("  ");

         -- Print the fraction.
         Fraction_IO.Put(Item => Byte_Data.Fraction, Fore => 4, Aft => 3);
         Ada.Text_IO.Put("  ");

         -- Print the code.
         Ada.Text_IO.Put(Ada.Strings.Unbounded.To_String(Byte_Data.Code));
         Ada.Text_IO.New_Line;
      end loop;
   end Print_Frequency_Table;

begin -- Huffman

   -- Check command line.
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put("Usage: ");
      Ada.Text_IO.Put(Ada.Command_Line.Command_Name);
      Ada.Text_IO.Put(" filename");
      Ada.Text_IO.New_Line;
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
   else
      Code_Tree.Read_File(Ada.Command_Line.Argument(1));
      Code_Tree.Build_Tree;
      Code_Tree.Assign_Codes;
      Print_Frequency_Table;
   end if;

end Huffman;
