---------------------------------------------------------------------------
--  FILE    : checksum.adb
--  SUBJECT : Program to explore the effectiveness of checksums
--
--  This program uses the simulated noisy channel to investigate various techniques for
--  computing checksums. It passes blocks of data over the channel and checks the result to see
--  if the checksum detects any errors that occurred.
--
--  Try running this program using a bit error rate of 1.0E-3 on 1_000_000 blocks. Try it again
--  with a bit error rate of 1.0E-4.
---------------------------------------------------------------------------

with Channel; use Channel;
with CRC;
with Ada.Text_IO; use Ada.Text_IO;

procedure Checksum is

   --  This defines a type representing a block of data (1024 bits).
   subtype Block_Index_Type is Integer range 0 .. 127;
   subtype Block is Channel.Octet_Array (Block_Index_Type);

   --  The results are collected in an error table.
   type Error_Table_Row is
      record
         Error_Occurrences : Natural := 0;
         Simple_Undetected : Natural := 0;
         CRC_Undetected    : Natural := 0;
      end record;

   subtype Bit_Error_Count is Integer range 0 .. 7;
   type Error_Table is array (Bit_Error_Count) of Error_Table_Row;

   Table            : Error_Table;  -- The table for the results.
   In_Block         : Block;        -- The uncorrupted input block.
   Out_Block        : Block;        -- The possibly corrupted output block.
   Simple_In_Check  : Double_Octet; -- The simple checksum of the input block.
   Simple_Out_Check : Double_Octet; -- The simple checksum of the output block.
   CRC_In_Check     : Double_Octet; -- The CRC checksum of the input block.
   CRC_Out_Check    : Double_Octet; -- The CRC checksum of the output block.
   Error_Rate       : Float;        -- The desired bit error rate.
   Block_Count      : Positive;     -- The total number of blocks to process.
   Error_Count      : Natural;      -- The total number of bit errors that occurred.

   --  Separate procedures and functions.
   procedure Generate_Data (Holding_Tank : in out Block) is separate;
   procedure Transmit_Data (Input : Block; Result : out Block) is separate;
   function Count_Errors (Original : Block; Perverted : Block) return Integer is separate;
   procedure Print_Table (Results : Error_Table) is separate;

   --  This function computes a simple checksum.
   function Compute_Checksum (Data : Block) return Double_Octet
     with Pre => Data'Length rem 2 = 0
   is
      Result : Double_Octet := 0;
   begin
      for I in 1 .. Data'Length / 2 loop
         Result :=
           Result +
             256 * Double_Octet (Data (Data'First + 2 * (I - 1))) +
               Double_Octet (Data (Data'First + 2 * (I - 1) + 1));
      end loop;
      return Result;
   end Compute_Checksum;

   --  This function computes the CRC checksum.
   function Compute_CRC_Checksum (Data : Block) return Double_Octet is
   begin
      return CRC.CRC_Calculation (Data);
   end Compute_CRC_Checksum;

   package Int_IO is new Integer_IO (Integer);
   package Flt_IO is new Float_IO (Float);

begin -- Checksum
   Channel.Initialize;

   Put ("Enter the bit error rate: ");
   Flt_IO.Get (Error_Rate);
   Skip_Line;
   Channel.Error_Rate (New_Rate => Error_Rate);

   Put ("Enter the number of blocks to process: ");
   Int_IO.Get (Block_Count);
   Skip_Line;

   for I in 1 .. Block_Count loop
      Generate_Data (In_Block);
      Transmit_Data (Input => In_Block, Result => Out_Block);
      Error_Count := Count_Errors (Original => In_Block, Perverted => Out_Block);

      --  The current code does not pass the checksum over the channel.
      Simple_In_Check  := Compute_Checksum (In_Block);
      Simple_Out_Check := Compute_Checksum (Out_Block);
      CRC_In_Check  := Compute_CRC_Checksum (In_Block);
      CRC_Out_Check := Compute_CRC_Checksum (Out_Block);

      if Error_Count <= Bit_Error_Count'Last then
         Table (Error_Count).Error_Occurrences :=
           Table (Error_Count).Error_Occurrences + 1;
      end if;

      if Simple_In_Check = Simple_Out_Check and Error_Count /= 0 then
         if Error_Count <= Bit_Error_Count'Last then
            Table (Error_Count).Simple_Undetected :=
              Table (Error_Count).Simple_Undetected + 1;
         end if;
      end if;

      if CRC_In_Check = CRC_Out_Check and Error_Count /= 0 then
         if Error_Count <= Bit_Error_Count'Last then
            Table (Error_Count).CRC_Undetected :=
              Table (Error_Count).CRC_Undetected + 1;
         end if;
      end if;

   end loop;

   Print_Table (Table);
end Checksum;
