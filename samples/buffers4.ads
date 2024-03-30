pragma SPARK_Mode(On);

package Buffers2 is

   Maximum_Buffer_Size : constant := 1024;
   subtype Buffer_Count_Type is Natural  range 0 .. Maximum_Buffer_Size;
   subtype Buffer_Index_Type is Positive range 1 .. Maximum_Buffer_Size;
   type    Buffer_Type       is array (Buffer_Index_Type) of Character;

   -- Fills Buffer with Fill_Character.
   procedure Fill(Buffer : out Buffer_Type; Fill_Character : in Character)
     with
       Depends => (Buffer => Fill_Character),
       Post => (for all I in Buffer_Index_Type => Buffer(I) = Fill_Character);

   -- Rotates Buffer contents right (toward higher index values).
   procedure Rotate_Right(Buffer : in out Buffer_Type; Distance : in Buffer_Count_Type)
     with
       Depends => (Buffer =>+ Distance),
       Post =>
         (for all I in Buffer_Index_Type =>
            Buffer(Buffer'First +
               (((I - Buffer'First) + Distance) mod Maximum_Buffer_Size)) = Buffer'Old(I));

   -- Reverses the contents of Buffer.
   procedure Reverse_Buffer(Buffer : in out Buffer_Type)
     with
       Depends => (Buffer =>+ null),
       Post =>
         (for all I in Buffer_Index_Type =>
            Buffer(I) = Buffer'Old(Buffer'Last - (I - Buffer'First)));

   -- Returns the number of occurrences of Ch in Buffer.
   function Count_Character
     (Buffer : in Buffer_Type; Ch : in Character) return Buffer_Count_Type;

   -- Returns the number of occurrences of Ch in Buffer and replaces those occurrences with ' '.
   procedure Count_And_Erase_Character
     (Buffer : in out Buffer_Type;
      Ch     : in     Character;
      Count  :    out Buffer_Count_Type)
     with
       Depends => ((Buffer, Count) => (Buffer, Ch)),
       Post =>
         (for all I in Buffer_Index_Type =>
            (if Buffer'Old(I) = Ch then Buffer(I) = ' '
                                   else Buffer(I) = Buffer'Old(I)));

   -- Removes instances of Erase_Character, compacting Buffer as needed. New space at the end is
   -- filled with Fill_Character. After returning, Valid contains a count of the remaining valid
   -- characters (not including the fill characters).
   procedure Compact
     (Buffer          : in out Buffer_Type;
      Erase_Character : in     Character;
      Fill_Character  : in     Character;
      Valid           :    out Buffer_Count_Type)
     with
       Depends => (Buffer =>+ (Erase_Character, Fill_Character),
                   Valid  =>  (Erase_Character, Buffer)),
       Post =>
         (for all I in Buffer'First + Valid .. Buffer'Last =>
            Buffer(I) = Fill_Character) and
         (for all I in Buffer'First .. Buffer'First + Valid - 1 =>
            Buffer(I) /= Erase_Character);

   -- Copies the source string into the buffer. If the source string is too short the buffer is
   -- padded with spaces. If the source string is too long, it is truncated.
   procedure Copy_Into(Buffer : out Buffer_Type; Source : in String)
     with
       Depends => (Buffer => Source),
       Post => (for all I in Buffer_Index_Type =>
                  (Buffer(I) = (if I  - Buffer'First < Source'Length
                                   then Source(Source'First + (I - Buffer'First))
                                   else ' ')));

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
       Depends => (Buffer =>+ (Source, Point, Length)),
       Post => (for all I in Buffer_Index_Type =>
                 Buffer(I) = (if I < Point
                                then Buffer'Old(I)
                                else (if I - Point < Buffer_Count_Type'Min(Length, Source'Length)
                                        then Source(Source'First + (I - Point))
                                        else Buffer'Old(I))));

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
       Depends => (Destination =>+ (Buffer, Point, Length)),
       Post => (for all I in Destination'Range =>
                  Destination(I) = (if I - Destination'First < Length and I - Destination'First < Buffer'Last - Point + 1
                                      then Buffer(Point + (I - Destination'First))
                                      else ' '));

end Buffers2;
