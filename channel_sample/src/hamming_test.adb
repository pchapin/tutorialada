with Text_IO; use Text_IO;
with Hamming;
with System;
-- with Util;

procedure Hamming_Test is

  Byte : Integer;
  package Int_IO is new Integer_IO (Integer); use Int_IO;

begin
  Put ("Enter a number (0 .. 255) ");
  Get (Byte);
  Byte := Hamming.Hamming_Encode (Byte);

  Put ("Total result obtained = ");
  Put (Item => Byte, Base => 16);
  New_Line;

  -- Put ("Encoded message       = ");
  -- Put (System.Byte'Pos (Util.Lo (Byte)));
  -- New_Line;
  --
  -- Put ("Code bits             = ");
  -- Put (System.Byte'Pos (Util.Hi (Byte)));
  -- New_Line;

end Hamming_Test;

