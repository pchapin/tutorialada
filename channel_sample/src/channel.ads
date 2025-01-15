---------------------------------------------------------------------------
-- FILE    : channel.ads
-- SUBJECT : Package specification for simulated noisy channel.
--
---------------------------------------------------------------------------

package Channel is

   type Octet is mod 2**8;
   type Double_Octet is mod 2**16;

   type Octet_Array is array (Natural range <>) of Octet;

   procedure Initialize;
   procedure Error_Rate(New_Rate : in Float);
   procedure Transceive(Data     : in out Octet);

end Channel;
