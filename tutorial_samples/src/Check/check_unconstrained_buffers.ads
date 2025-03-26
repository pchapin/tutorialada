---------------------------------------------------------------------------
-- FILE    : check_unconstrained_buffers.ads
-- SUBJECT : Package containing tests of package Unconstrained_Buffers.
--
---------------------------------------------------------------------------
with AUnit;
with AUnit.Test_Cases;

use AUnit.Test_Cases;

package Check_Unconstrained_Buffers is

   type Unconstrained_Buffer_Test is new AUnit.Test_Cases.Test_Case with null record;

   overriding procedure Register_Tests(T : in out Unconstrained_Buffer_Test);
   overriding function Name(T : Unconstrained_Buffer_Test) return AUnit.Message_String;

end Check_Unconstrained_Buffers;
