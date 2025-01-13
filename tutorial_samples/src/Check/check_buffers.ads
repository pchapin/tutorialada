---------------------------------------------------------------------------
--  FILE    : check_buffers.ads
--  SUBJECT : Package containing tests of package Buffers.
--
---------------------------------------------------------------------------
with AUnit;
with AUnit.Test_Cases;

use AUnit.Test_Cases;

package Check_Buffers is

   type Buffer_Test is new AUnit.Test_Cases.Test_Case with null record;

   overriding procedure Register_Tests(T : in out Buffer_Test);
   overriding function Name(T : Buffer_Test) return AUnit.Message_String;

end Check_Buffers;
