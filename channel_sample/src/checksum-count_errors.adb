
separate (Checksum)

function Count_Errors (Original : Block; Perverted : Block) return Integer is
   Bad_Bits   : Integer := 0;
   Error_Byte : Channel.Octet;

   Masks : constant array (0 .. 7) of Octet :=
     (16#01#, 16#02#, 16#04#, 16#08#, 16#10#, 16#20#, 16#40#, 16#80#);
begin

   for I in Block'Range loop
      Error_Byte := Original (I) xor Perverted (I);
      if Error_Byte /= 0 then

         for Bit_Number in 0 .. 7 loop
            if (Error_Byte and Masks (Bit_Number)) /= 0 then
               Bad_Bits := Bad_Bits + 1;
            end if;
         end loop;

      end if;
   end loop;

   return Bad_Bits;

end Count_Errors;
