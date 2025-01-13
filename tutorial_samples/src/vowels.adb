---------------------------------------------------------------------------
-- FILE    : vowels.adb
-- SUBJECT : This program counts the vowels in the input file.
--
-- The character 'Y' is handled as a special case. 'Y' is sometimes a vowel and sometimes not
-- depending on context. A correct vowel counting program would need to consider this issue and
-- would be much more complicated.
--
-- Exercise: Enhance this program to count (or not) occurrences of Y appropriately.
---------------------------------------------------------------------------

-- Used in the tutorial section on control structures...
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Vowels is
   Letter      : Character;
   Vowel_Count : Integer := 0;
   Y_Count     : Integer := 0;
begin
   while not End_Of_File loop
      Get(Letter);
      case Letter is
         when 'A'|'E'|'I'|'O'|'U' |
              'a'|'e'|'i'|'o'|'u' =>
            Vowel_Count := Vowel_Count + 1;

         when 'Y'|'y' =>
            Y_Count := Y_Count + 1;

         when others =>
            null;
      end case;
   end loop;
   Put("Total number of vowels = "); Put(Vowel_Count); New_Line;
   Put("Total number of Ys = "); Put(Y_Count); New_Line;
end Vowels;
