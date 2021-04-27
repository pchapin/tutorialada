---------------------------------------------------------------------------
-- FILE          : checksum.adb
-- LAST REVISION : 2021-04-27
-- SUBJECT       : Program to explore the effectiveness of checksums
-- PROGRAMMER    : (C) Copyright 2021 by Peter C. Chapin
--
-- This program uses the simulated noisy channel to investigate various checksumming techniques. It passes
-- blocks of data over the channel and checks the result to see if the checksum detects any errors that occured.
-- Currently this program only uses a simple checksum. However, it could be enhanced to also investigate CRC
-- checksums and parity (as well as other, more sophisticated techniques such as Hamming error correcting
-- codes).
---------------------------------------------------------------------------

with Channel; use Channel;
with Ada.Text_IO; use Ada.Text_IO;

procedure checksum is

   -- This defines a type representing a block of data.
   subtype Block_Index is Integer range 0..31;
   type Block is array(Block_Index) of Channel.Octet;

   -- The results are collected in an error table.
   type Error_Table_Row is
      record
         Error_Occurrences      : Integer := 0;
         Undetected_Occurrences : Integer := 0;
      end record;

   subtype Bit_Error_Count is Integer range 0..7;
   type Error_Table is array(Bit_Error_Count) of Error_Table_Row;

   Table       : Error_Table;  -- The table for the results.
   In_Block    : Block;        -- The uncorrupted input block.
   Out_Block   : Block;        -- The possibly corrupted output block.
   In_Check    : Octet;        -- The checksum of the input block.
   Out_Check   : Octet;        -- The checksum of the output block.
   Error_Rate  : Float;        -- The desired bit error rate.
   Block_Count : Positive;     -- The total number of blocks to process.
   Error_Count : Natural;      -- The total number of bit errors that occured.

   -- Separate procedures and functions.
   procedure Generate_Data(Holding_Tank : in out Block) is separate;
   procedure Transmit_Data(Input : in Block; Result : out Block) is separate;
   function Count_Errors(Original : in Block; Perverted : in Block) return Integer is separate;
   procedure Print_Table(Results : in Error_Table) is separate;

   -- This function computes a simple checksum.
   function Compute_Checksum(Data : in Block) return Octet is
      Result : Octet := 0;
   begin
      for I in Block'Range loop
         Result := Result + Data(I);
      end loop;
      return Result;
   end Compute_Checksum;

   package Int_IO is new Integer_IO(Integer);
   package Flt_IO is new Float_IO(Float);

begin -- Checksum
   Channel.Initialize;

   Put("Enter the bit error rate: ");
   Flt_IO.Get(Error_Rate);
   Skip_Line;
   Channel.Error_Rate(New_Rate => Error_Rate);

   Put("Enter the number of blocks to process: ");
   Int_IO.Get(Block_Count);
   Skip_Line;

   for I in 1 .. Block_Count loop
      Generate_Data(In_Block);
      Transmit_Data(Input => In_Block, Result => Out_Block);
      Error_Count := Count_Errors(Original => In_Block, Perverted => Out_Block);

      -- The current code does not pass the checksum over the channel.
      In_Check  := Compute_Checksum(In_Block);
      Out_Check := Compute_Checksum(Out_Block);
      if (In_Check = Out_Check and Error_Count /= 0) then
         if (Error_Count <= Bit_Error_Count'last) then
            Table(Error_Count).Undetected_Occurrences :=
              Table(Error_Count).Undetected_Occurrences + 1;
         end if;
      end if;

      if (Error_Count <= Bit_Error_Count'last) then
         Table(Error_Count).Error_Occurrences :=
           Table(Error_Count).Error_Occurrences + 1;
      end if;
   end loop;

   Print_Table(Table);
end checksum;
