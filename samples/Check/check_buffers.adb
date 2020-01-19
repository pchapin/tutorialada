---------------------------------------------------------------------------
-- FILE    : check_buffers.adb
-- SUBJECT : Package containing tests of package Buffers.
-- AUTHOR  : (C) Copyright 2015 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Ada.Strings.Unbounded;
with AUnit.Assertions;
with Buffers;

use Ada.Strings.Unbounded;
use AUnit.Assertions;
use Buffers;

package body Check_Buffers is

   --------------
   -- Fill_Buffer
   --------------

   procedure Test_Fill(T : in out Test_Case'Class) is
      Buffer1 : Buffer_Type( 1 ..  0);  -- Empty.
      Buffer2 : Buffer_Type( 1 ..  1);  -- Single element.
      Buffer3 : Buffer_Type(10 .. 10);  -- Single element with "unusual" index bounds.
      Buffer4 : Buffer_Type( 1 .. 10);  -- Typical case.
      Buffer5 : Buffer_Type(11 .. 20);  -- Typical case with "unusual" index bounds.

      Result1 : constant Buffer_Type( 1 ..  0) := (others => 'x');
      Result2 : constant Buffer_Type( 1 ..  1) := (others => 'x');
      Result3 : constant Buffer_Type(10 .. 10) := (others => 'x');
      Result4 : constant Buffer_Type( 1 .. 10) := (others => 'x');
      Result5 : constant Buffer_Type(11 .. 20) := (others => 'x');
   begin
      Fill(Buffer1, 'x');
      Fill(Buffer2, 'x');
      Fill(Buffer3, 'x');
      Fill(Buffer4, 'x');
      Fill(Buffer5, 'x');

      Assert(Buffer1 = Result1, "Fill of Buffer1 failed");
      Assert(Buffer2 = Result2, "Fill of Buffer2 failed");
      Assert(Buffer3 = Result3, "Fill of Buffer3 failed");
      Assert(Buffer4 = Result4, "Fill of Buffer4 failed");
      Assert(Buffer5 = Result5, "Fill of Buffer5 failed");
   end Test_Fill;


   -----------------
   -- Reverse_Buffer
   -----------------

   -- This is a very basic test. It's better than nothing!
   procedure Test_Reverse_Buffer(T : in out Test_Case'Class) is
      Workspace : Buffer_Type(10 .. 20);
   begin
      Workspace(Workspace'First) := 'X';
      Workspace(Workspace'Last ) := 'Y';
      Workspace(Workspace'First + Workspace'Length/2 - 1) := 'A';
      Workspace(Workspace'Last  - Workspace'Length/2 + 1) := 'B';
      Reverse_Buffer(Workspace);
      Assert(Workspace(Workspace'First) = 'Y' and Workspace(Workspace'Last) = 'X',
             "Endpoints not reversed");
      Assert
        (Workspace(Workspace'First + Workspace'Length/2 - 1) = 'B' and
         Workspace(Workspace'Last  - Workspace'Length/2 + 1) = 'A', "Midpoints not reversed");
      -- Should also verify that the remaining array elements are still spaces.
   end Test_Reverse_Buffer;


   -------------------------------
   -- Rotate_Right and Rotate_Left
   -------------------------------

   type Rotate_Record is
      record
         Buffer : Buffer_Type(11 .. 15);
         Distance : Buffer_Count_Type;
         Expected : Buffer_Type(11 .. 15);
      end record;

   Rotate_Right_Cases : constant array(1 .. 6) of Rotate_Record :=
     ((Buffer => ('1', '2', '3', '4', '5'), Distance =>   0, Expected => ('1', '2', '3', '4', '5')),
      (Buffer => ('1', '2', '3', '4', '5'), Distance =>   1, Expected => ('5', '1', '2', '3', '4')),
      (Buffer => ('1', '2', '3', '4', '5'), Distance =>   4, Expected => ('2', '3', '4', '5', '1')),
      (Buffer => ('1', '2', '3', '4', '5'), Distance =>   5, Expected => ('1', '2', '3', '4', '5')),
      (Buffer => ('1', '2', '3', '4', '5'), Distance =>   6, Expected => ('5', '1', '2', '3', '4')),
      (Buffer => ('1', '2', '3', '4', '5'), Distance => 100, Expected => ('1', '2', '3', '4', '5'))
     );

   Rotate_Left_Cases : constant array(1 .. 6) of Rotate_Record :=
     ((Buffer => ('1', '2', '3', '4', '5'), Distance =>   0, Expected => ('1', '2', '3', '4', '5')),
      (Buffer => ('1', '2', '3', '4', '5'), Distance =>   1, Expected => ('2', '3', '4', '5', '1')),
      (Buffer => ('1', '2', '3', '4', '5'), Distance =>   4, Expected => ('5', '1', '2', '3', '4')),
      (Buffer => ('1', '2', '3', '4', '5'), Distance =>   5, Expected => ('1', '2', '3', '4', '5')),
      (Buffer => ('1', '2', '3', '4', '5'), Distance =>   6, Expected => ('2', '3', '4', '5', '1')),
      (Buffer => ('1', '2', '3', '4', '5'), Distance => 100, Expected => ('1', '2', '3', '4', '5'))
     );

   procedure Test_Rotate_Right(T : in out Test_Case'Class) is
      Buffer_Empty : Buffer_Type(1 .. 0);
      Buffer_Single : Buffer_Type(2 .. 2) := (2 => 'x');
      Workspace : Buffer_Type(11 .. 15);
   begin
      Rotate_Right(Buffer_Empty, 1);
      Assert(Buffer_Empty = Buffer_Empty, "*"); -- If we get here there was no exception.
      Rotate_Right(Buffer_Single, 2);
      Assert(Buffer_Single = (1 => 'x'), "Rotate_Right of single element buffer failed");

      for I in Rotate_Right_Cases'Range loop
         Workspace := Rotate_Right_Cases(I).Buffer;
         Rotate_Right(Workspace, Rotate_Right_Cases(I).Distance);
         Assert(Workspace = Rotate_Right_Cases(I).Expected,
                "Rotate_Right fails for case " & Integer'Image(I));
      end loop;
   end Test_Rotate_Right;


   procedure Test_Rotate_Left(T : in out Test_Case'Class) is
      Buffer_Empty : Buffer_Type(1 .. 0);
      Buffer_Single : Buffer_Type(2 .. 2) := (2 => 'x');
      Workspace : Buffer_Type(11 .. 15);
   begin
      Rotate_Left(Buffer_Empty, 1);
      Assert(Buffer_Empty = Buffer_Empty, "*"); -- If we get here there was no exception.
      Rotate_Left(Buffer_Single, 2);
      Assert(Buffer_Single = (1 => 'x'), "Rotate_Left of single element buffer failed");

      for I in Rotate_Left_Cases'Range loop
         Workspace := Rotate_Left_Cases(I).Buffer;
         Rotate_Left(Workspace, Rotate_Left_Cases(I).Distance);
         Assert(Workspace = Rotate_Left_Cases(I).Expected,
                "Rotate_Left fails for case " & Integer'Image(I));
      end loop;
   end Test_Rotate_Left;


   -------------------
   -- Count_Substrings
   -------------------

   -- This is a very basic test. It's better than nothing!
   procedure Test_Count_Substrings(T : in out Test_Case'Class) is
      Workspace : constant Buffer_Type(101 .. 106) := ('a', 'b', 'a', 'b', 'a', 'b');
   begin
      Assert(Count_Substrings(Workspace, "aba") = 2, "Failed Test #1");
   end Test_Count_Substrings;


   ------------------
   -- Count_Character
   ------------------

   type Count_Character_Record is
      record
         Input : Buffer_Type(101 .. 200);
         Ch : Character;
         Expected_Count : Buffer_Count_Type;
      end record;

   -- The array positions and distances should not be hard coded values.
   Count_Character_Cases : constant array(1 .. 4) of Count_Character_Record :=
     (1 => (Input => (                        others => ' '), Ch => 'X', Expected_Count => 0),
      2 => (Input => (101 => 'X',             others => ' '), Ch => 'X', Expected_Count => 1),
      3 => (Input => (101 => 'X', 200 => 'X', others => ' '), Ch => 'X', Expected_Count => 2),
      4 => (Input => (                        others => 'X'), Ch => 'X', Expected_Count => 100)
     );

   procedure Test_Count_Character(T : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      for I in Count_Character_Cases'Range loop
         Assert
           (Count_Character(Count_Character_Cases(I).Input, Count_Character_Cases(I).Ch) =
              Count_Character_Cases(I).Expected_Count,
            "Count_Character FAILS for case #" & Integer'Image(I));
      end loop;
   end Test_Count_Character;


   ----------------------------
   -- Count_And_Erase_Character
   ----------------------------

    type Count_And_Erase_Character_Record is
      record
         Input : Buffer_Type(101 .. 200);
         Ch : Character;
         Expected_Count : Buffer_Count_Type;
         Expected_Output : Buffer_Type(101 .. 200);
      end record;

   type CAECC_Type is array(1 .. 4) of Count_And_Erase_Character_Record;
   Count_And_Erase_Character_Cases : constant CAECC_Type :=
     (1 => (Input           => (others => '-'),
            Ch              => 'X',
            Expected_Count  => 0,
            Expected_Output => (others => '-')),

      2 => (Input           => (101 => 'X', others => '-'),
            Ch              => 'X',
            Expected_Count  => 1,
            Expected_Output => (101 => ' ', others => '-')),

      3 => (Input           => (101 => 'X', 200 => 'X', others => '-'),
            Ch              => 'X',
            Expected_Count  => 2,
            Expected_Output => (101 => ' ', 200 => ' ', others => '-')),

      4 => (Input           => (others => 'X'),
            Ch              => 'X',
            Expected_Count  => 100,
            Expected_Output => (others => ' '))
     );

   procedure Test_Count_And_Erase_Character(T : in out AUnit.Test_Cases.Test_Case'Class) is
      CAECC : CAECC_Type renames Count_And_Erase_Character_Cases;
      Workspace : Buffer_Type(101 .. 200);
      Count : Buffer_Count_Type;
   begin
      for I in CAECC'Range loop
         Workspace := CAECC(I).Input;
         Count_And_Erase_Character(Workspace, CAECC(I).Ch, Count);
         Assert(Workspace = CAECC(I).Expected_Output and Count = CAECC(I).Expected_Count,
                "Count_And_Erase_Character FAILS for case #" & Integer'Image(I));
      end loop;
   end Test_Count_And_Erase_Character;


   ----------
   -- Compact
   ----------

   type Compact_Record is
      record
         Input : Buffer_Type(101 .. 200);
         Erase : Character;
         Fill : Character;
         Expected_Output : Buffer_Type(101 .. 200);
         Expected_Valid : Buffer_Count_Type;
      end record;

   Compact_Cases : constant array(1 .. 4) of Compact_Record :=
     (1 => (Input           => (others => ' '),
            Erase           => ' ',
            Fill            => 'X',
            Expected_Output => (others => 'X'),
            Expected_Valid  => 0),

      2 => (Input           => (others => '-'),
            Erase           => ' ',
            Fill            => 'X',
            Expected_Output => (others => '-'),
            Expected_Valid  => 100),

      3 => (Input           => (101 => ' ', others => '-'),
            Erase           => ' ',
            Fill            => 'X',
            Expected_Output => (200 => 'X', others => '-'),
            Expected_Valid  => 99),

      4 => (Input           => (102 => ' ', 104 => ' ', others => '-'),
            Erase           => ' ',
            Fill            => 'X',
            Expected_Output => (199 => 'X', 200 => 'X', others => '-'),
            Expected_Valid  => 98)
     );

   procedure Test_Compact(T : in out AUnit.Test_Cases.Test_Case'Class) is
      Workspace : Buffer_Type(101 .. 200);
      Valid     : Buffer_Count_Type;
   begin
      for I in Compact_Cases'Range loop
         Workspace := Compact_Cases(I).Input;
         Compact(Workspace, Compact_Cases(I).Erase, Compact_Cases(I).Fill, Valid);
         Assert
           (Workspace = Compact_Cases(I).Expected_Output and
                Valid = Compact_Cases(I).Expected_Valid,
            "Compact FAILS for case #" & Integer'Image(I));
      end loop;
   end Test_Compact;


   ------------
   -- Copy_Into
   ------------

   procedure Test_Copy_Into(T : in out AUnit.Test_Cases.Test_Case'Class) is
      subtype Test_Buffer_Type is Buffer_Type(101 .. 200);
      Workspace : Test_Buffer_Type;
      Oversize  : constant String(1 .. 101) := (101 => 'y', others => 'x');
   begin
      Copy_Into(Workspace, "");
      Assert(Workspace = Test_Buffer_Type'(others => ' '),
             "Copy_Into FAILS for an empty string");

      Copy_Into(Workspace, "x");
      Assert(Workspace = Test_Buffer_Type'(101 => 'x', others => ' '),
             "Copy_Into FAILS for single character string");

      Copy_Into(Workspace, String'(1 .. 100 => 'x'));
      Assert(Workspace = Test_Buffer_Type'(others => 'x'),
             "Copy_Into FAILS for full size string");

      Copy_Into(Workspace, Oversize);
      Assert(Workspace = Test_Buffer_Type'(others => 'x'),
             "Copy_Into FAILS for oversize string");
   end Test_Copy_Into;


   ------------
   -- Copy_Onto
   ------------

   type Copy_Onto_Record is
      record
         Input : Buffer_Type(101 .. 200);
         Source : Unbounded_String;
         Point : Buffer_Index_Type;
         Length : Buffer_Count_Type;
         Expected : Buffer_Type(101 .. 200);
      end record;

   Copy_Onto_Cases : constant array(1 .. 5) of Copy_Onto_Record :=
     (1 => (Input    => (others => ' '),
            Source   => To_Unbounded_String(""),
            Point    => 101,
            Length   => 2,
            Expected => (others => ' ')),

      2 => (Input    => (others => ' '),
            Source   => To_Unbounded_String("x"),
            Point    => 101,
            Length   => 1,
            Expected => (101 => 'x', others => ' ')),

      3 => (Input    => (others => ' '),
            Source   => To_Unbounded_String("x"),
            Point    => 101,
            Length   => 2,
            Expected => (101 => 'x', others => ' ')),

      4 => (Input    => (others => ' '),
            Source   => To_Unbounded_String("xy"),
            Point    => 199,
            Length   => 2,
            Expected => (199 => 'x', 200 => 'y', others => ' ')),

      5 => (Input    => (others => ' '),
            Source   => To_Unbounded_String("xy"),
            Point    => 200,
            Length   => 3,
            Expected => (200 => 'x', others => ' '))
     );

   procedure Test_Copy_Onto(T : in out AUnit.Test_Cases.Test_Case'Class) is
      Workspace : Buffer_Type(101 .. 200);
   begin
      for I in Copy_Onto_Cases'Range loop
         Workspace := Copy_Onto_Cases(I).Input;
         Buffers.Copy_Onto
           (Workspace,
            To_String(Copy_Onto_Cases(I).Source),
            Copy_Onto_Cases(I).Point,
            Copy_Onto_Cases(I).Length);
         Assert(Workspace = Copy_Onto_Cases(I).Expected,
                "Copy_Onto FAILS for case #" & Integer'Image(I));
      end loop;
   end Test_Copy_Onto;


   ------------
   -- Substring
   ------------

   type Substring_Record is
      record
         Input : Buffer_Type(101 .. 200);
         Point : Buffer_Index_Type;
         Length : Buffer_Count_Type;
         Expected : String(1 .. 100);
         Expected_Length : Natural;
      end record;

   -- TODO: We are in desperate need of more test cases here!
   Substring_Cases : constant array(1 .. 2) of Substring_Record :=
     (1 => (Input    => (101 => 'x', others => ' '),
            Point    => 101,
            Length   => 1,
            Expected => (1 => 'x', others => ' '),
            Expected_Length => 1),

      2 => (Input    => (101 => 'x', 102 => 'y', others => ' '),
            Point    => 101,
            Length   => 2,
            Expected => (1 => 'x', 2 => 'y', others => ' '),
            Expected_Length => 2)
     );

   procedure Test_Substring(T : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      for I in Substring_Cases'Range loop
         declare
            Current : Substring_Record renames Substring_Cases(I);
            Result : constant String := Substring(Current.Input, Current.Point, Current.Length);
         begin
            Assert(Result'Length = Current.Expected_Length,
                   "Substring FAILS for case #" & Integer'Image(I));

            for J in Current.Point .. Current.Point + Current.Expected_Length - 1 loop
               Assert(Current.Input(J) = Result((J - Current.Point) + 1),
                      "Substring FAILS for case #" & Integer'Image(I));
            end loop;
         end;
      end loop;
   end Test_Substring;


   procedure Register_Tests(T : in out Buffer_Test) is
   begin
      Registration.Register_Routine(T, Test_Fill'Access, "Fill");
      Registration.Register_Routine(T, Test_Reverse_Buffer'Access, "Reverse_Buffer");
      Registration.Register_Routine(T, Test_Rotate_Right'Access, "Rotate_Right");
      Registration.Register_Routine(T, Test_Rotate_Left'Access, "Rotate_Left");
      Registration.Register_Routine(T, Test_Count_Substrings'Access, "Count_Substrings");
      Registration.Register_Routine(T, Test_Count_Character'Access, "Count_Character");
      Registration.Register_Routine(T, Test_Count_And_Erase_Character'Access, "Count_And_Erase_Character");
      Registration.Register_Routine(T, Test_Compact'Access, "Compact");
      Registration.Register_Routine(T, Test_Copy_Into'Access, "Copy_Into");
      Registration.Register_Routine(T, Test_Copy_Onto'Access, "Copy_Onto");
      Registration.Register_Routine(T, Test_Substring'Access, "Substring");
   end Register_Tests;


   function Name(T : Buffer_Test) return AUnit.Message_String is
   begin
      return AUnit.Format("Buffers");
   end Name;

end Check_Buffers;
