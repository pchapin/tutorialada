
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;

separate (Checksum)

procedure Print_Table (Results : Error_Table) is

   Block_Count : Integer := 0;

   package Int_IO is new Integer_IO (Integer);
   package Flt_IO is new Float_IO (Float);

   function Pad (Header : String; Size : Positive := Integer'Width) return String is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      Result : String (1 .. Size);
   begin
      Move (Source  => Header,
            Target  => Result,
            Drop    => Error,
            Justify => Right,
            Pad     => Space);
      return Result;
   end Pad;

   Separator_Length : constant Integer := 4 * Integer'Width + Float'Width + 4 * 4;
   Separator_Line   : constant String (1 .. Separator_Length) := (others => '=');

begin -- Print_Table

   for I in Bit_Error_Count loop
      Block_Count := Block_Count + Results (I).Error_Occurrences;
   end loop;

   --  Print the table header
   New_Line (2);
   Put (Pad ("#Errors"));
   Put ("    ");
   Put (Pad ("#Times"));
   Put ("    ");
   Put (Pad ("Probability", Float'Width));
   Put ("    ");
   Put (Pad ("#SimpUndet"));
   Put ("    ");
   Put (Pad ("#CRCUndet"));
   New_Line;
   Put_Line (Separator_Line);

   --  Print the table body.
   for I in Bit_Error_Count loop
      Int_IO.Put (I);
      Put ("    ");
      Int_IO.Put (Results (I).Error_Occurrences);
      Put ("    ");
      Flt_IO.Put (Float (Results (I).Error_Occurrences) / Float (Block_Count));
      Put ("    ");
      Int_IO.Put (Results (I).Simple_Undetected);
      Put ("    ");
      Int_IO.Put (Results (I).CRC_Undetected);
      New_Line;
   end loop;
end Print_Table;
