---------------------------------------------------------------------------
-- FILE   : bit_files.adb
-- SUBJECT: Implementation of the bit file handling package.
--
---------------------------------------------------------------------------

package body Bit_Files is

   procedure Create(File : in out File_Type; Name : in String) is
   begin
      Byte_IO.Create(File.Underlying_File, Byte_IO.Out_File, Name);
      File.Mode       := Out_File;
      File.Bit_Buffer := 0;
      File.Bit_Index  := 0;
   end Create;

   procedure Open(File : in out File_Type; Mode : in File_Mode; Name : in String) is
      Underlying_Mode : Byte_IO.File_Mode;
   begin
      if Mode = In_File then
         Underlying_Mode := Byte_IO.In_File;
      else
         Underlying_Mode := Byte_IO.Out_File;
      end if;
      Byte_IO.Open(File.Underlying_File, Underlying_Mode, Name);
      File.Mode       := Mode;
      File.Bit_Buffer := 0;
      File.Bit_Index  := 0;
   end Open;

   procedure Put (File : in out File_Type; Bit : in Bit_Type) is
      use type Interfaces.Unsigned_8;
   begin
      File.Bit_Buffer :=
         File.Bit_Buffer or Interfaces.Shift_Left(Interfaces.Unsigned_8(Bit), File.Bit_Index);

      if File.Bit_Index = 7 then
         Byte_IO.Write(File.Underlying_File, File.Bit_Buffer);
         File.Bit_Buffer := 0;
         File.Bit_Index  := 0;
      else
         File.Bit_Index := File.Bit_Index + 1;
      end if;
   end Put;

   procedure Get(File : in out File_Type; Bit : out Bit_Type) is
      use type Interfaces.Unsigned_8;
   begin
      --  Fetch a fresh byte if necessary.
      if File.Bit_Index = 0 then
         Byte_IO.Read(File.Underlying_File, File.Bit_Buffer);
      end if;

      --  Extract a bit.
      if (File.Bit_Buffer and
            Interfaces.Shift_Left(Interfaces.Unsigned_8'(1), File.Bit_Index)) /= 0
      then
         Bit := 1;
      else
         Bit := 0;
      end if;

      --  Advance Bit_Index.
      if File.Bit_Index = 7 then
         File.Bit_Index := 0;
      else
         File.Bit_Index := File.Bit_Index + 1;
      end if;
   end Get;


   procedure Close(File : in out File_Type) is
   begin
      if File.Mode = Out_File and File.Bit_Index /= 0 then
         Byte_IO.Write(File.Underlying_File, File.Bit_Buffer);
      end if;
      Byte_IO.Close(File.Underlying_File);
   end Close;

end Bit_Files;
