---------------------------------------------------------------------------
-- FILE    : samples_test.adb
-- SUBJECT : Main procedure of the samples unit test program.
--
-- This program uses the AUnit framework to run unit tests for certain samples.
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
