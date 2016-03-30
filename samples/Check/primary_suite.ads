---------------------------------------------------------------------------
-- FILE    : primary_suite.ads
-- SUBJECT : The main test suite of the samples unit test program.
-- AUTHOR  : (C) Copyright 2015 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with AUnit.Test_Suites;

package Primary_Suite is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;
end Primary_Suite;
