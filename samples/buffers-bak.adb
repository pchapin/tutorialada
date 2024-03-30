---------------------------------------------------------------------------
-- FILE    : buffers.adb
-- SUBJECT : Body of a package providing various buffer manipulating subprograms.
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

package body Buffers.Bak is

   procedure Fill(Buffer : out Buffer_Type; Fill_Character : in Character) is
   begin
      Buffer := (others => Fill_Character);
   end Fill;


   procedure Reverse_Buffer(Buffer : in out Buffer_Type) is
      Temp : Character;
   begin
      for I in Buffer_Index_Type range Buffer'First .. (Buffer'First + Buffer'Length/2) - 1 loop
         Temp := Buffer(I);
         Buffer(I)  := Buffer(Buffer'Last - (I - Buffer'First));
         Buffer(Buffer'Last - (I - Buffer'First)) := Temp;
      end loop;
   end Reverse_Buffer;


   procedure Rotate_Right(Buffer : in out Buffer_Type; Distance : in Buffer_Count_Type) is
   begin
      if Buffer'Length = 0 then
         return;
      end if;

      for I in 1 .. Distance loop
         Buffer := Buffer(Buffer'Last) & Buffer(Buffer'First .. Buffer'Last - 1);
      end loop;
   end Rotate_Right;


   procedure Rotate_Left(Buffer : in out Buffer_Type; Distance : in Buffer_Count_Type) is
      Temp : Character;
   begin
      if Buffer'Length = 0 then
         return;
      end if;

      for I in 1 .. Distance loop
         Temp := Buffer(Buffer'First);
         Buffer(Buffer'First .. Buffer'Last - 1) := Buffer(Buffer'First + 1 .. Buffer'Last);
         Buffer(Buffer'Last) := Temp;
      end loop;
   end Rotate_Left;


   function Count_Substrings(Buffer : Buffer_Type; Search : String) return Buffer_Count_Type is
      Result    : Buffer_Count_Type := 0;
      Positions : Buffer_Index_Type;
      Matched   : Boolean;
   begin
      if Search'Length > 0 and Buffer'Length >= Search'Length then
         Positions := (Buffer'Length - Search'Length) + 1;
         for I in 1 .. Positions loop
            pragma Loop_Invariant(Result < I);

            -- Does Search match the buffer's contents starting at position I?
            Matched := True;
            for J in Search'First .. Search'Last loop
               if Search(J) /= Buffer(((J - Search'First) + (I - 1)) + Buffer'First) then
                 Matched := False;
               end if;
            end loop;

            -- If we matched, count it.
            if Matched then
               Result := Result + 1;
            end if;
         end loop;
      end if;
      return Result;
   end Count_Substrings;


   function Count_Character(Buffer : Buffer_Type; Ch : Character) return Buffer_Count_Type is
      Counter : Buffer_Count_Type := 0;
   begin
      for I in Buffer'Range loop
         pragma Loop_Invariant(Counter < I);
         if Buffer(I) = Ch then
            Counter := Counter + 1;
         end if;
      end loop;
      return Counter;
   end Count_Character;


   procedure Count_And_Erase_Character
     (Buffer : in out Buffer_Type;
      Ch     : in Character;
      Count  : out Buffer_Count_Type) is
   begin
      Count := 0;
      for I in Buffer'Range loop
         pragma Loop_Invariant(Count < I);
         if Buffer(I) = Ch then
            Count := Count + 1;
            Buffer(I) := ' ';
         end if;
      end loop;
   end Count_And_Erase_Character;


   procedure Compact
     (Buffer          : in out Buffer_Type;
      Erase_Character : in     Character;
      Fill_Character  : in     Character;
      Valid           :    out Buffer_Count_Type) is

      Workspace : Buffer_Type(Buffer'Range);
      Offset    : Buffer_Count_Type;
   begin
      Workspace := (others => ' ');
      Offset := 0;

      -- Take interesting characters from the original buffer and pack them into Workspace.
      for I in Buffer'Range loop
         pragma Loop_Invariant(Offset < (I - Buffer'First) + 1);
         if Buffer(I) /= Erase_Character then
            Workspace(Workspace'First + Offset) := Buffer(I);
            Offset := Offset + 1;
         end if;
      end loop;

      Valid := Offset;

      -- Install appropriate number of fill characters.
      for I in Offset .. Buffer'Length - 1 loop
         Workspace(Workspace'First + I) := Fill_Character;
      end loop;
      Buffer := Workspace;
   end Compact;


   procedure Copy_Into(Buffer : out Buffer_Type; Source : in String) is
      Characters_To_Copy : Buffer_Count_Type := Buffer'Length;
   begin
      Buffer := (others => ' ');

      if Source'Length < Characters_To_Copy then
         Characters_To_Copy := Source'Length;
      end if;
      for I in 1 .. Characters_To_Copy loop
         Buffer(Buffer'First + (I - 1)) := Source(Source'First + (I - 1));
      end loop;
   end Copy_Into;


   procedure Copy_Onto
     (Buffer : in out Buffer_Type;
      Source : in     String;
      Point  : in     Buffer_Index_Type;
      Length : in     Buffer_Count_Type) is

      Characters_To_Copy : Buffer_Count_Type := Length;
   begin
      -- If Point is outside the buffer there is no effect.
      if Point not in Buffer'Range then
         return;
      end if;

      -- If the source string is longer than Length, it is truncated.
      if Source'Length < Characters_To_Copy then
         Characters_To_Copy := Source'Length;
      end if;

      -- If the requested length goes beyond the end of the buffer, it is truncated.
      if Point + (Characters_To_Copy - 1) > Buffer'Last then
         Characters_To_Copy := (Buffer'Last - Point) + 1;
      end if;

      for I in 1 .. Characters_To_Copy loop
         Buffer(Point + (I - 1)) := Source(Source'First + (I - 1));
      end loop;
   end Copy_Onto;


   function Substring
     (Buffer : Buffer_Type;
      Point  : Buffer_Index_Type;
      Length : Buffer_Count_Type) return String is

      Characters_To_Copy : Buffer_Count_Type := Length;
   begin
       -- If Point is outside the buffer, an empty string is returned.
      if Point not in Buffer'Range then
         return "";
      end if;

      -- If the requested length goes beyond the end of the buffer, it is truncated.
      if Point + (Characters_To_Copy - 1) > Buffer'Last then
         Characters_To_Copy := (Buffer'Last - Point) + 1;
      end if;

      declare
         Result_Length : constant Buffer_Count_Type := Characters_To_Copy;
         Result : String(1 .. Result_Length) := (others => ' ');
      begin
         for I in 1 .. Characters_To_Copy loop
            Result(I) := Buffer(Point + (I - 1));
         end loop;
         return Result;
      end;
   end Substring;


end Buffers.Bak;
