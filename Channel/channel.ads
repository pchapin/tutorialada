---------------------------------------------------------------------------
-- FILE          : channel.ads
-- LAST REVISION : 2021-04-27
-- SUBJECT       : Package specification for simulated noisy channel.
-- PROGRAMMER    : (C) Copyright 2021 by Peter C. Chapin
---------------------------------------------------------------------------

package Channel is

   type Octet is mod 256;

   procedure Initialize;
   procedure Error_Rate(New_Rate : in     Float);
   procedure Transceive(Data     : in out Octet);

end Channel;
