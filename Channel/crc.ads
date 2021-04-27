with Channel; use Channel;

package CRC is

   -- Computes the CRC checksum of the data in Buffer.
   function CRC_Calculation
     (Buffer : Octet_Array) return Double_Octet;

   -- For large data sets (or data that isn't all available at once), the CRC can be continued by passing
   -- the result of a previous call of CRC_Calculation or Continuation_CRC_Calculation into this function
   -- along with *additional* data to be covered by the CRC checksum.
   function Continuation_CRC_Calculation
     (Buffer : Octet_Array;
      Seed   : Double_Octet) return Double_Octet;

end CRC;
