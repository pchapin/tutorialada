---------------------------------------------------------------------------
-- FILE          : channel.ads
-- LAST REVISION : 2003-12-28
-- SUBJECT       : Package specification for simulated noisy channel.
-- PROGRAMMER    : (C) Copyright 2003 by Peter C. Chapin
---------------------------------------------------------------------------

package Channel is

   type Octet is mod 256;

   procedure Initialize;
   procedure Error_Rate(New_Rate : IN     Float);
   procedure Transceive(Data     : IN OUT Octet);

end Channel;
