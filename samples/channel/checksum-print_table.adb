
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;

separate(Checksum)
procedure Print_Table(Results : IN Error_Table) is

   Block_Count : Integer := 0;

   package Int_IO is new Integer_IO(Integer);
   package Flt_IO is new Float_IO(Float);

   function Pad(Header: in String; Size: Positive := Integer'Width) return
     String is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      Result: String(1..Size);
   begin
      Move(Source  => Header,
           Target  => Result,
           Drop    => Error,
           Justify => Right,
           Pad     => Space);
      return Result;
   end Pad;

   Separator_Length : Integer := 3*Integer'Width + Float'Width + 3*4;
   Separator_Line   : String(1..Separator_Length) := (others => '=');

begin -- Print_Table

   for I in Bit_Error_Count loop
      Block_Count := Block_Count + Results(I).Error_Occurrences;
   end loop;

   -- Print the table header
   New_Line(2);
   Put(Pad("#Errors"));
   Put("    ");
   Put(Pad("#Times"));
   Put("    ");
   Put(Pad("Probability", Float'Width));
   Put("    ");
   Put(Pad("#Undetected"));
   New_Line;
   Put_Line(Separator_Line);

   -- Print the table body.
   for I in Bit_Error_Count loop
      Int_IO.Put(I);
      Put("    ");
      Int_IO.Put(Results(I).Error_Occurrences);
      Put("    ");
      Flt_IO.Put(Float(Results(I).Error_Occurrences)/Float(Block_Count));
      Put("    ");
      Int_IO.Put(Results(I).Undetected_Occurrences);
      New_Line;
   end loop;
end Print_Table;
