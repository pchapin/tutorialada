---------------------------------------------------------------------------
-- FILE    : check_sorters.ads
-- SUBJECT : Package containing tests of package Sorters.
-- AUTHOR  : (C) Copyright 2012 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with AUnit;
with AUnit.Test_Cases;

package Check_Sorters is

   type Sort_Test is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests(T : in out Sort_Test);
   function Name(T : Sort_Test) return AUnit.Message_String;

end Check_Sorters;
