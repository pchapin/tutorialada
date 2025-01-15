---------------------------------------------------------------------------
-- FILE   : hex.ads
-- SUBJECT: Specification of a package for manipulating hex strings.
--
---------------------------------------------------------------------------

with Interfaces;

package Hex is
   -- Convert an 8 bit quantity to a two digit hex string.
   function To_String(Value : Interfaces.Unsigned_8) return String;

   -- Convert a 32 bit quantity to a four digit hex string.
   function To_String(Value : Interfaces.Unsigned_32) return String;
end Hex;
