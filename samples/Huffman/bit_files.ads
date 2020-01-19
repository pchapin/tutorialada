---------------------------------------------------------------------------
-- FILE    : bit_file.ads
-- SUBJECT : Interface to a bit file handling package.
-- AUTHOR  : (C) Copyright 2015 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <chapinp@acm.org>
---------------------------------------------------------------------------
private with Ada.Sequential_IO;
private with Interfaces;

package Bit_Files is

   type File_Type is limited private;
   type File_Mode is (In_File, Out_File);
   subtype Bit_Type is Natural range 0 .. 1;

   -- Create a new bit file in output mode using the given string as the file's name.
   procedure Create(File : in out File_Type; Name : String);

   -- Open an existing, named file as a bit file using the given mode.
   procedure Open(File : in out File_Type; Mode : in  File_Mode; Name : String);

   -- Write a bit into the specified bit file.
   procedure Put(File : in out File_Type; Bit  : in  Bit_Type);

   -- Read a bit from the specified bit file.
   procedure Get(File : in out File_Type; Bit  : out Bit_Type);

   -- Close the specified bit file. If the file was open for output, the final bits are flushed.
   -- The last byte is zero-filled if necessary.
   --
   procedure Close(File : in out File_Type);

private
   package Byte_IO is new Ada.Sequential_IO(Interfaces.Unsigned_8);

   type File_Type is
      record
         Underlying_File : Byte_IO.File_Type;
         Mode            : File_Mode;
         Bit_Buffer      : Interfaces.Unsigned_8 := 0;
         Bit_Index       : Natural range 0 .. 7 := 0;
      end record;
end Bit_Files;
