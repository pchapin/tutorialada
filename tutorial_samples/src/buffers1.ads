---------------------------------------------------------------------------
-- FILE    : buffers1.ads
-- SUBJECT : Package providing various buffer manipulating subprograms.
--
-- A Buffer is essentially a string bounded in length to 1024 characters. However, unlike the
-- Standard.String type, Buffer_Type has subprograms more in keeping with use as a storage area
-- for raw data.
--
-- This version does not use SPARK.
---------------------------------------------------------------------------

package Buffers1 is

   Maximum_Buffer_Size : constant := 1024;
   subtype Buffer_Count_Type is Natural  range 0 .. Maximum_Buffer_Size;
   subtype Buffer_Index_Type is Positive range 1 .. Maximum_Buffer_Size;
   type    Buffer_Type       is array (Buffer_Index_Type) of Character;

   -- Fills Buffer with Fill_Character.
   procedure Fill(Buffer : out Buffer_Type; Fill_Character : in Character);

   -- Rotates Buffer contents right (toward higher index values).
   procedure Rotate_Right(Buffer : in out Buffer_Type; Distance : in Buffer_Count_Type);

   -- Reverses the contents of Buffer.
   procedure Reverse_Buffer(Buffer : in out Buffer_Type);

   -- Returns the number of occurrences of Ch in Buffer.
   function Count_Character(Buffer : in Buffer_Type; Ch : in Character) return Buffer_Count_Type;

   -- Returns the number of occurrences of Ch in Buffer and replaces those occurrences with ' '.
   procedure Count_And_Erase_Character
     (Buffer : in out Buffer_Type;
      Ch     : in     Character;
      Count  :    out Buffer_Count_Type);

   -- Removes instances of Erase_Character, compacting Buffer as needed. New space at the end
   -- is filled with Fill_Character. After returning, Valid contains a count of the remaining
   -- valid characters (i.e., not including the fill characters).
   procedure Compact
     (Buffer          : in out Buffer_Type;
      Erase_Character : in     Character;
      Fill_Character  : in     Character;
      Valid           :    out Buffer_Count_Type);

   -- Copies the source string into Buffer. If the source string is too short Buffer is padded
   -- with spaces. If the source string is too long, it is truncated.
   procedure Copy_Into(Buffer : out Buffer_Type; Source : in String);

   -- Copies the source string onto Buffer starting at position Point. At most, Length characters
   -- are copied. If the source string is longer then Length, it is truncated. If the requested
   -- length goes beyond the end of Buffer it is truncated. Characters not overwritten are
   -- retained without change.
   procedure Copy_Onto
     (Buffer : in out Buffer_Type;
      Source : in     String;
      Point  : in     Buffer_Index_Type;
      Length : in     Buffer_Count_Type);

   -- Copies the substring of Buffer starting at Point into the destination string. If the
   -- requested length goes beyond the end of Buffer it is truncated. If the destination is
   -- too short only the characters it can hold are copied. If the destination is too long it
   -- is padded (on the right) with spaces.
   procedure Copy_From
     (Buffer      : in     Buffer_Type;
      Destination :    out String;
      Point       : in     Buffer_Index_Type;
      Length      : in     Buffer_Count_Type);

   -- EXERCISES
   ------------

   -- Complete the implementations of the subprograms below. Solutions are in Buffers2.adb.

   -- Rotates Buffer contents left (toward lower index values);
   procedure Rotate_Left(Buffer : in out Buffer_Type; Distance : in Buffer_Count_Type);

   -- Returns a count of the number of times the string Search appears in Buffer. For example,
   -- if Buffer contains "ababab" and Search is "aba" this function should return 2.
   function Count_Substrings(Buffer : in Buffer_Type; Search : in String) return Buffer_Count_Type;

   -- MORE EXERCISES (no solutions given)
   -----------------

   -- Implement the following subprograms, then copy your implementations to Buffers2, Buffers3,
   -- and Buffers4 with the following adjustments:
   --
   -- Buffers2: Add Global and Depends aspects and use SPARK to show consistent flows.
   -- Buffers3: Prove your implementation has AORTE.
   -- Buffers4: Add suitable postconditions and prove your implementation satisfies them.
   --
   -- If you would prefer, you can copy your implementations directly to Buffers4 and enhance
   -- them according to the steps above.

   -- Overlay the buffer with repeating copies of Overlay_Text. The last copy can be a partial
   -- copy, if necessary. For example, If the Overlay_Text is "Hello", the buffer should become
   -- "HelloHelloHelloHello ... Hell". In other words, the last copy of "Hello" is truncated.
   procedure Overlay(Buffer : out Buffer_Type; Overlay_Text : in String);

   -- Scan the buffer looking for the first character that is out of sorted (ascending) order.
   -- Return the index position of that character or 0 if the array is entirely sorted.
   function Find_Unsorted(Buffer : in Buffer_Type) return Buffer_Count_Type;

   -- Sorts the characters in the buffer in ascending order. Use a simple sorting algorithm
   -- such as Bubble Sort (to make things easier later).
   procedure Sort(Buffer : in out Buffer_Type);

end Buffers1;
