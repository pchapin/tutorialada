---------------------------------------------------------------------------
--  FILE    : channel.adb
--  SUBJECT : Package body for simulated noisy channel.
--
---------------------------------------------------------------------------

with Ada.Numerics.Float_Random;

package body Channel is

   --  My random number generator.
   Gen : Ada.Numerics.Float_Random.Generator;

   --  Current bit error rate. This can be adjusted by calls to Error_Rate.
   Bit_Error_Rate : Float := 1.0e-3;

   --  Set up the channel.
   procedure Initialize is
   begin
      Ada.Numerics.Float_Random.Reset (Gen);
   end Initialize;

   --  Allows clients to set the bit error rate. Note: no checking on range.
   procedure Error_Rate (New_Rate : Float) is
   begin
      Bit_Error_Rate := New_Rate;
   end Error_Rate;

   --  Corrupts bits in Data at rate given by Bit_Error_Rate.
   procedure Transceive (Data : in out Octet) is
      Dice  : Float;
      Masks : constant array (0 .. 7) of Octet :=
        (16#01#, 16#02#, 16#04#, 16#08#, 16#10#, 16#20#, 16#40#, 16#80#);
   begin
      for I in 0 .. 7 loop
         Dice := Ada.Numerics.Float_Random.Random (Gen);
         if Dice >= 0.5 and Dice < 0.5 + Bit_Error_Rate then
            Data := Data xor Masks (I);
         end if;
      end loop;
   end Transceive;

end Channel;
