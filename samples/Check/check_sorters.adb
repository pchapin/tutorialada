---------------------------------------------------------------------------
-- FILE    : check_sorters.adb
-- SUBJECT : Package containing tests of package Sorters.
-- AUTHOR  : (C) Copyright 2012 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with AUnit.Assertions; use AUnit.Assertions;
with Sorters;

package body Check_Sorters is
   use type Sorters.Array_Type;

   type Sort_Record is
      record
         Input           : Sorters.Array_Type;
         Expected_Output : Sorters.Array_Type;
      end record;

   Sort_Cases : constant array(1 .. 5) of Sort_Record :=
     (
       1 => (Input           => (16, 15, 14, 13, 12, 11, 10,  9,  8,  7,  6,  5,  4,  3,  2,  1),
             Expected_Output => ( 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16)),

       2 => (Input           => ( 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16),
             Expected_Output => ( 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16)),

       3 => (Input           => ( 2,  2,  2,  2,  2,  2,  2,  2,  1,  1,  1,  1,  1,  1,  1,  1),
             Expected_Output => ( 1,  1,  1,  1,  1,  1,  1,  1,  2,  2,  2,  2,  2,  2,  2,  2)),

       4 => (Input           => ( 2,  1,  2,  1,  2,  1,  2,  1,  2,  1,  2,  1,  2,  1,  2,  1),
             Expected_Output => ( 1,  1,  1,  1,  1,  1,  1,  1,  2,  2,  2,  2,  2,  2,  2,  2)),

       5 => (Input           => ( 1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1),
             Expected_Output => ( 1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1))
     );


   procedure Test_Selection_Sort(T : in out AUnit.Test_Cases.Test_Case'Class) is
      Workspace : Sorters.Array_Type;
   begin
      for I in Sort_Cases'Range loop
         Workspace := Sort_Cases(I).Input;
         -- TODO: Try a few cases where Limit /= Workspace'Last.
         Sorters.Selection_Sort(Workspace, Workspace'Last);
         Assert(Workspace = Sort_Cases(I).Expected_Output, "Sorters.Selection_Sort FAILS for case #" & Integer'Image(I));
      end loop;
   end Test_Selection_Sort;


   procedure Test_Bubble_Sort(T : in out AUnit.Test_Cases.Test_Case'Class) is
      Workspace : Sorters.Array_Type;
   begin
      for I in Sort_Cases'Range loop
         Workspace := Sort_Cases(I).Input;
         Sorters.Bubble_Sort(Workspace);
         Assert(Workspace = Sort_Cases(I).Expected_Output, "Sorters.Bubble_Sort FAILS for case #" & Integer'Image(I));
      end loop;
   end Test_Bubble_Sort;


   procedure Test_Merge_Sort(T : in out AUnit.Test_Cases.Test_Case'Class) is
      Workspace : Sorters.Array_Type;
   begin
      for I in Sort_Cases'Range loop
         Workspace := Sort_Cases(I).Input;
         Sorters.Merge_Sort(Workspace);
         Assert(Workspace = Sort_Cases(I).Expected_Output, "Sorters.Merge_Sort FAILS for case #" & Integer'Image(I));
      end loop;
   end Test_Merge_Sort;


   procedure Register_Tests(T : in out Sort_Test) is
   begin
      AUnit.Test_Cases.Registration.Register_Routine(T, Test_Selection_Sort'Access, "Selection_Sort");
      AUnit.Test_Cases.Registration.Register_Routine(T, Test_Bubble_Sort'Access, "Bubble_Sort");
      AUnit.Test_Cases.Registration.Register_Routine(T, Test_Merge_Sort'Access, "Merge_Sort");
   end Register_Tests;


   function Name(T : Sort_Test) return AUnit.Message_String is
   begin
      return AUnit.Format("Sorters");
   end Name;

end Check_Sorters;
