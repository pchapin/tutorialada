---------------------------------------------------------------------------
-- FILE    : check_number_theory.ads
-- SUBJECT : Package containing tests of package Number_Theory.
-- AUTHOR  : (C) Copyright 2015 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with AUnit;
with AUnit.Test_Cases;

use AUnit.Test_Cases;

package Check_Number_Theory is

   type Prime_Test is new Test_Case with null record;

   procedure Register_Tests(T : in out Prime_Test);
   function Name(T : Prime_Test) return AUnit.Message_String;

end Check_Number_Theory;
