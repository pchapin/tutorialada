---------------------------------------------------------------------------
-- FILE    : check_buffers.ads
-- SUBJECT : Package containing tests of package Buffers.
-- AUTHOR  : (C) Copyright 2015 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with AUnit;
with AUnit.Test_Cases;

use AUnit.Test_Cases;

package Check_Buffers is

   type Buffer_Test is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests(T : in out Buffer_Test);
   function Name(T : Buffer_Test) return AUnit.Message_String;

end Check_Buffers;
