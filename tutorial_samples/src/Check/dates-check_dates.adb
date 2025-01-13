---------------------------------------------------------------------------
--  FILE    : dates-check_dates.adb
--  SUBJECT : Package containing tests of package Dates.
--
---------------------------------------------------------------------------
with AUnit.Assertions; use AUnit.Assertions;

package body Dates.Check_Dates is

   ----------
   --  Create
   ----------

   type Create_Record is
      record
         Y      : Year_Type;
         M      : Month_Type;
         D      : Day_Type;
      end record;

   Create_Success_Cases : constant array (1 .. 29) of Create_Record :=
     ( 1 => (Y => 2000, M =>  1, D =>  1),  -- Check a few boundaries.
       2 => (Y => 2000, M =>  2, D => 29),
       3 => (Y => 2000, M => 12, D => 31),
       4 => (Y => 2001, M =>  1, D =>  1),
       5 => (Y => 2099, M => 12, D => 31),
       6 => (Y => 2001, M =>  1, D => 31),  -- Check last day of every month for a non-leap year.
       7 => (Y => 2001, M =>  2, D => 28),
       8 => (Y => 2001, M =>  3, D => 31),
       9 => (Y => 2001, M =>  4, D => 30),
      10 => (Y => 2001, M =>  5, D => 31),
      11 => (Y => 2001, M =>  6, D => 30),
      12 => (Y => 2001, M =>  7, D => 31),
      13 => (Y => 2001, M =>  8, D => 31),
      14 => (Y => 2001, M =>  9, D => 30),
      15 => (Y => 2001, M => 10, D => 31),
      16 => (Y => 2001, M => 11, D => 30),
      17 => (Y => 2001, M => 12, D => 31),
      18 => (Y => 2004, M =>  1, D => 31),  -- Check last day of every month for a leap year.
      19 => (Y => 2004, M =>  2, D => 29),
      20 => (Y => 2004, M =>  3, D => 31),
      21 => (Y => 2004, M =>  4, D => 30),
      22 => (Y => 2004, M =>  5, D => 31),
      23 => (Y => 2004, M =>  6, D => 30),
      24 => (Y => 2004, M =>  7, D => 31),
      25 => (Y => 2004, M =>  8, D => 31),
      26 => (Y => 2004, M =>  9, D => 30),
      27 => (Y => 2004, M => 10, D => 31),
      28 => (Y => 2004, M => 11, D => 30),
      29 => (Y => 2004, M => 12, D => 31));

   Create_Failure_Cases : constant array (1 .. 9) of Create_Record :=
     ( 1 => (Y => 2000, M =>  2, D => 30),  -- The only failure cases are for over-large days.
       2 => (Y => 2000, M =>  2, D => 31),
       3 => (Y => 2000, M =>  4, D => 31),
       4 => (Y => 2000, M =>  6, D => 31),
       5 => (Y => 2000, M =>  9, D => 31),
       6 => (Y => 2000, M => 11, D => 31),
       7 => (Y => 2001, M =>  2, D => 29),
       8 => (Y => 2001, M =>  2, D => 30),
       9 => (Y => 2001, M =>  2, D => 31));

   procedure Test_Create (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Result : Date;
      Valid  : Boolean;
   begin
      for I in Create_Success_Cases'Range loop
         Create_Date
           (Create_Success_Cases (I).Y,
            Create_Success_Cases (I).M,
            Create_Success_Cases (I).D, Result, Valid);
         Assert
           (Get_Year (Result)  = Create_Success_Cases (I).Y and
            Get_Month (Result) = Create_Success_Cases (I).M and
            Get_Day (Result)   = Create_Success_Cases (I).D and Valid,
            "Dates.Create FAILS for case #" & Integer'Image(I));
      end loop;

      for I in Create_Failure_Cases'Range loop
         Create_Date
           (Create_Failure_Cases (I).Y,
            Create_Failure_Cases (I).M,
            Create_Failure_Cases (I).D, Result, Valid);
         Assert (not Valid, "Dates.Create FAILS to fail for case #" & Integer'Image (I));
      end loop;
   end Test_Create;

   ----------
   -- Advance
   ----------

   type Advance_Record is
      record
         Start    : Date;
         Distance : Day_Advance_Type;
         Expected : Date;
      end record;

   Advance_Success_Cases : constant array (1 .. 5) of Advance_Record :=
     (1 => (Start    => (Year => 2000, Month =>  1, Day =>  1),
            Distance => 1,
            Expected => (Year => 2000, Month =>  1, Day =>  2)),

      2 => (Start    => (Year => 2000, Month =>  2, Day => 28),
            Distance => 1,
            Expected => (Year => 2000, Month =>  2, Day => 29)),

      3 => (Start    => (Year => 2000, Month =>  2, Day => 29),
            Distance => 1,
            Expected => (Year => 2000, Month =>  3, Day =>  1)),

      4 => (Start    => (Year => 2000, Month =>  1, Day =>  1),
            Distance => Day_Advance_Type'Last,
            Expected => (Year => 2099, Month => 12, Day => 31)),

      5 => (Start    => (Year => 2099, Month => 12, Day => 31),
            Distance => Day_Advance_Type'First,
            Expected => (Year => 2000, Month =>  1, Day =>  1))
     );

   Advance_Failure_Cases : constant array (1 .. 4) of Advance_Record :=
     (1 => (Start    => (Year => 2099, Month => 12, Day => 31),
            Distance => 1,
            Expected => (Year => 2099, Month => 12, Day => 31)),

      2 => (Start    => (Year => 2000, Month =>  1, Day =>  2),
            Distance => Day_Advance_Type'Last,
            Expected => (Year => 2099, Month => 12, Day => 31)),

      3 => (Start    => (Year => 2000, Month =>  1, Day =>  1),
            Distance => -1,
            Expected => (Year => 2000, Month =>  1, Day =>  1)),

      4 => (Start    => (Year => 2099, Month => 12, Day => 30),
            Distance => Day_Advance_Type'First,
            Expected => (Year => 2000, Month =>  1, Day =>  1))
     );

   procedure Test_Advance (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Temporary_Date : Date;
      Valid          : Boolean;
   begin
      for I in Advance_Success_Cases'Range loop
         Temporary_Date := Advance_Success_Cases (I).Start;
         Advance (Temporary_Date, Advance_Success_Cases (I).Distance, Valid);
         Assert
           (Temporary_Date = Advance_Success_Cases (I).Expected and Valid,
            "Dates.Advance FAILS for case #" & Integer'Image (I));
      end loop;

      for I in Advance_Failure_Cases'Range loop
         Temporary_Date := Advance_Failure_Cases (I).Start;
         Advance (Temporary_Date, Advance_Failure_Cases (I).Distance, Valid);
         Assert
           (Temporary_Date = Advance_Failure_Cases (I).Expected and (not Valid),
            "Dates.Advance FAILS to fail for case #" & Integer'Image (I));
      end loop;
   end Test_Advance;

   overriding
   procedure Register_Tests (T : in out Date_Test) is
   begin
      AUnit.Test_Cases.Registration.Register_Routine (T, Test_Create'Access, "Create");
      AUnit.Test_Cases.Registration.Register_Routine (T, Test_Advance'Access, "Advance");
   end Register_Tests;

   overriding
   function Name (T : Date_Test) return AUnit.Message_String is
   begin
      return AUnit.Format ("Dates");
   end Name;

end Dates.Check_Dates;