---------------------------------------------------------------------------
--  FILE    : check_sorters.ads
--  SUBJECT : Package containing tests of package Sorters.
--
---------------------------------------------------------------------------
with AUnit;
with AUnit.Test_Cases;

package Check_Sorters is

   type Sort_Test is new AUnit.Test_Cases.Test_Case with null record;

   overriding procedure Register_Tests (T : in out Sort_Test);
   overriding function Name (T : Sort_Test) return AUnit.Message_String;

end Check_Sorters;
