---------------------------------------------------------------------------
-- FILE          : hex.adb
-- SUBJECT       : Implementation of a package for manipulating hex strings.
-- PROGRAMMER    : (C) Copyright 2006 by Peter C. Chapin
---------------------------------------------------------------------------
with Interfaces; use Interfaces;

package body Hex is

   Conversion_Table : constant array(Unsigned_8 range 0..15) of Character :=
     ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');

   procedure Flip(Buffer : in out String) is
      Temp : Character;
      J    : Positive;
   begin
      for I in Buffer'First .. Buffer'First + Buffer'Length/2 - 1 loop
         J := Buffer'Last - (I - Buffer'First);
         Temp := Buffer(I);
         Buffer(I) := Buffer(J);
         Buffer(J) := Temp;
      end loop;
   end Flip;

   function To_String(Value : Interfaces.Unsigned_8) return String is
      Result : String(1 .. 2);
   begin
      Result(1) := Conversion_Table(Value mod 16);
      Result(2) := Conversion_Table(Value /   16);
      Flip(Result);
      return Result;
   end To_String;

   function To_String(Value : Interfaces.Unsigned_32) return String is
      Result : String(1 .. 8);
      Temp   : Unsigned_32 := Value;
   begin
      for I in 1 .. 8 loop
         Result(I) := Conversion_Table(Unsigned_8(Temp mod 16));
         Temp := Temp / 16;
      end loop;
      Flip(Result);
      return Result;
   end To_String;

end Hex;
