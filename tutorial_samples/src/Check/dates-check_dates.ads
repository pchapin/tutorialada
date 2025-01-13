---------------------------------------------------------------------------
--  FILE    : dates-check_dates.ads
--  SUBJECT : Package containing tests of package Dates.
--
---------------------------------------------------------------------------
with AUnit;
with AUnit.Test_Cases;

package Dates.Check_Dates is

   type Date_Test is new AUnit.Test_Cases.Test_Case with null record;

   overriding procedure Register_Tests (T : in out Date_Test);
   overriding function Name (T : Date_Test) return AUnit.Message_String;

end Dates.Check_Dates;
