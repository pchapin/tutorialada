---------------------------------------------------------------------------
--  FILE    : primary_suite.adb
--  SUBJECT : The main test suite of the samples unit test program.
--
---------------------------------------------------------------------------
with Check_Buffers;
with Dates.Check_Dates;
with Check_Sorters;

package body Primary_Suite is
   use AUnit.Test_Suites;

   --  The suite itself.
   Suite_Object : aliased Test_Suite;

   --  The various tests in this suite.
   Test_1 : aliased Check_Buffers.Buffer_Test;
   Test_2 : aliased Dates.Check_Dates.Date_Test;
   Test_3 : aliased Check_Sorters.Sort_Test;

   --  Function to return an access to the configured suite
   function Suite return Access_Test_Suite is
   begin
      Add_Test (Suite_Object'Access, Test_1'Access);
      Add_Test (Suite_Object'Access, Test_2'Access);
      Add_Test (Suite_Object'Access, Test_3'Access);
      return Suite_Object'Access;
   end Suite;

end Primary_Suite;
