---------------------------------------------------------------------------
-- FILE    : primary_suite.ads
-- SUBJECT : The main test suite of the samples unit test program.
--
---------------------------------------------------------------------------
with AUnit.Test_Suites;

package Primary_Suite is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;
end Primary_Suite;
