---------------------------------------------------------------------------
-- FILE    : samples_test.adb
-- SUBJECT : Main procedure of the SPARK samples unit test program.
-- AUTHOR  : (C) Copyright 2012 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with AUnit.Run;
with AUnit.Reporter.Text;

with Primary_Suite;

procedure Samples_Test is
   procedure Run is new AUnit.Run.Test_Runner(Primary_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run(Reporter);
end Samples_Test;
