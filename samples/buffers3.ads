---------------------------------------------------------------------------
-- FILE    : buffers3.ads
-- SUBJECT : Package providing various buffer manipulating subprograms.
--
-- A Buffer is essentially a string bounded in length to 1024 characters. However, unlike the
-- Standard.String type, Buffer_Type has subprograms more in keeping with use as a storage area
-- for raw data.
--
-- This version uses SPARK to show absence of runtime errors (AORTE).
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

package Buffers3 is

   Maximum_Buffer_Size : constant := 1024;
   subtype Buffer_Count_Type is Natural  range 0 .. Maximum_Buffer_Size;
   subtype Buffer_Index_Type is Positive range 1 .. Maximum_Buffer_Size;
   type    Buffer_Type       is array (Buffer_Index_Type) of Character;

   -- Fills Buffer with Fill_Character.
   procedure Fill(Buffer : out Buffer_Type; Fill_Character : in Character)
     with
       Global => null,
       Depends => (Buffer => Fill_Character);

   -- Rotates Buffer contents right (toward higher index values).
   procedure Rotate_Right(Buffer : in out Buffer_Type; Distance : in Buffer_Count_Type)
     with
       Global => null,
       Depends => (Buffer =>+ Distance);

   -- Reverses the contents of Buffer.
   procedure Reverse_Buffer(Buffer : in out Buffer_Type)
     with
       Global => null,
       Depends => (Buffer =>+ null);

   -- Returns the number of occurrences of Ch in Buffer.
   function Count_Character(Buffer : in Buffer_Type; Ch : in Character) return Buffer_Count_Type
     with Global => null;

   -- Returns the number of occurrences of Ch in Buffer and replaces those occurrences with ' '.
   procedure Count_And_Erase_Character
     (Buffer : in out Buffer_Type;
      Ch     : in     Character;
      Count  :    out Buffer_Count_Type)
     with
       Global => null,
       Depends => ((Buffer, Count) => (Buffer, Ch));

   -- Removes instances of Erase_Character, compacting Buffer as needed. New space at the end is
   -- filled with Fill_Character. After returning, Valid contains a count of the remaining valid
   -- characters (not including the fill characters).
   procedure Compact
     (Buffer          : in out Buffer_Type;
      Erase_Character : in     Character;
      Fill_Character  : in     Character;
      Valid           :    out Buffer_Count_Type)
     with
       Global => null,
       Depends => (Buffer =>+ (Erase_Character, Fill_Character),
                   Valid  =>  (Erase_Character, Buffer));

   -- Copies the source string into the buffer. If the source string is too short the buffer is
   -- padded with spaces. If the source string is too long, it is truncated.
   procedure Copy_Into(Buffer : out Buffer_Type; Source : in String)
     with
       Global => null,
       Depends => (Buffer => Source);

   -- Copies the source string onto the buffer starting at position Point. At most Length
   -- characters are copied. If the source string is longer then Length, it is truncated. If the
   -- requested length goes beyond the end of the buffer it is truncated. Characters not
   -- overwritten are retained without change.
   procedure Copy_Onto
     (Buffer : in out Buffer_Type;
      Source : in     String;
      Point  : in     Buffer_Index_Type;
      Length : in     Buffer_Count_Type)
     with
       Global => null,
       Depends => (Buffer =>+ (Source, Point, Length));

   -- Copies the substring of the buffer starting at Point into the destination string. If the
   -- requested length goes beyond the end of the buffer it is truncated. If the destination is
   -- too short only the characters it can hold are copied. If the destination is too long it is
   -- padded (on the right) with spaces.
   procedure Copy_From
     (Buffer      : in     Buffer_Type;
      Destination :    out String;
      Point       : in     Buffer_Index_Type;
      Length      : in     Buffer_Count_Type)
     with
       Global => null,
       Depends => (Destination =>+ (Buffer, Point, Length));

   -- EXERCISES
   ------------

   -- Modify the implementations of the subprograms below so that AORTE can be proved.

   -- Rotates Buffer contents left (toward lower index values);
   procedure Rotate_Left(Buffer : in out Buffer_Type; Distance : in Buffer_Count_Type)
     with
       Global => null,
       Depends => (Buffer =>+ Distance);

   -- Returns a count of the number of times the string Search appears in Buffer. For example,
   -- if Buffer contains "ababab" and Search is "aba" this function should return 2.
   function Count_Substrings(Buffer : in Buffer_Type; Search : in String) return Buffer_Count_Type
     with Global => null;

end Buffers3;
