---------------------------------------------------------------------------
-- FILE    : dates-check_dates.ads
-- SUBJECT : Package containing tests of package Dates.
-- AUTHOR  : (C) Copyright 2012 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with AUnit;
with AUnit.Test_Cases;

package Dates.Check_Dates is

   type Date_Test is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests(T : in out Date_Test);
   function Name(T : Date_Test) return AUnit.Message_String;

end Dates.Check_Dates;
