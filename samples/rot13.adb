---------------------------------------------------------------------------
-- FILE          : rot13.adb
-- SUBJECT       : Program that does ROT13 transformation on a text file.
---------------------------------------------------------------------------
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;

procedure Rot13 is
   Lookup_Table : array(Character) of Character;

   -- Creates the lookup table using ROT13 on upper and lower case letters.
   procedure Initialize_Table is
      Transformed : Character;
   begin
      for Ch in Character loop
         Lookup_Table(Ch) := Ch;
      end loop;
      Transformed := 'N';
      for Ch in Character range 'A' .. 'Z' loop
         Lookup_Table(Ch) := Transformed;
         if Transformed = 'Z' then
            Transformed := 'A';
         else
            Transformed := Character'Succ(Transformed);
         end if;
      end loop;
      Transformed := 'n';
      for Ch in Character range 'a' .. 'z' loop
         Lookup_Table(Ch) := Transformed;
         if Transformed = 'z' then
            Transformed := 'a';
         else
            Transformed := Character'Succ(Transformed);
         end if;
      end loop;
   end Initialize_Table;

   I_File : File_Type;
   O_File : File_Type;
   Ch     : Character;

begin -- Rot13

   -- Check command line.
   if Argument_Count /= 2 then
      Put_Line("Usage: rot13 input_file output_file");
      Set_Exit_Status(1);
      return;
   end if;

   -- Open the files.
   Open  (File => I_File, Mode => In_File, Name => Argument(1));
   Create(File => O_File,                  Name => Argument(2));

   Initialize_Table;

   -- Process the file.
   while not End_Of_File(I_File) loop
      while not End_Of_Line(I_File) loop
         Get(I_File, Ch);
         Put(O_File, Lookup_Table(Ch));
      end loop;
      Skip_Line(I_File);
      New_Line(O_File);
   end loop;

   -- Clean up.
   Close(I_File);
   Close(O_File);
   Set_Exit_Status(0);
end Rot13;
