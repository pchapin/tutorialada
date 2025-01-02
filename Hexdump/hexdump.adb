---------------------------------------------------------------------------
-- FILE      : hexdump.adb
-- SUBJECT   : Simple hex file viewer utility.
-- PROGRAMMER: (C) Copyright 2006 by Peter C. Chapin
--
---------------------------------------------------------------------------

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Sequential_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Hex;

procedure Hexdump is
   package Byte_IO is new Ada.Sequential_IO(Unsigned_8);

   procedure Process_File(Name : String) is
      Input_File : Byte_IO.File_Type;
      Item       : Unsigned_8;
      Item_Count : Unsigned_32 := 0;

      subtype Line_Size_Type is Integer range 0 .. 16;
      Line_Size  : Line_Size_Type := 0;

      Text_Form  : String(1..16) := (others => '.');

   begin -- Process_File
      Byte_IO.Open(Input_File, Byte_IO.In_File, Name);
      while not Byte_IO.End_Of_File(Input_File) loop
         Byte_IO.Read(Input_File, Item);

         -- If at the beginning of the line, print file offset.
         if Line_Size = 0 then
            Put(Hex.To_String(Item_Count));
            Put(": ");
         end if;

         -- Print item.
         Item_Count := Item_Count + 1;
         Line_Size  := Line_Size + 1;
         if Item >= 16#20# and Item <= 16#7E# then
            Text_Form(Line_Size) := Character'Val(Item);
         end if;
         Put(Hex.To_String(Item));
         if Line_Size = 8 then
            Put(" - ");
         else
            Put(" ");
         end if;

         -- Go to next line if necessary.
         if Line_Size = Line_Size_Type'Last then
            Put("|"); Put(Text_Form); Put("|"); New_Line;
            Line_Size := Line_Size_Type'First;
            Text_Form := (others => '.');
         end if;
      end loop;
      Byte_IO.Close(Input_File);
   end Process_File;

begin -- Hexdump

   -- Check command line.
   if Argument_Count = 0 then
      Put("Usage: ");
      Put(Command_Name);
      Put(" filename ...");
      New_Line;
      Set_Exit_Status(Failure);
   else
      -- Process each file name on the command line.
      for I in 1..Argument_Count loop
         Put("Dump of "); Put(Argument(I)); New_Line(2);
         Process_File(Argument(I));
         New_Line(2); Put("end"); New_Line;
      end loop;
   end if;
end Hexdump;
