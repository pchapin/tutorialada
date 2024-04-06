pragma SPARK_Mode(On);

package body Buffers2 is

   procedure Fill(Buffer : out Buffer_Type; Fill_Character : in Character) is
   begin
      Buffer := (others => Fill_Character);
   end Fill;


   procedure Rotate_Right(Buffer : in out Buffer_Type; Distance : in Buffer_Count_Type) is
      Workspace : Buffer_Type := (others => ' ');
      New_Index : Buffer_Index_Type;
   begin
      for I in Buffer_Index_Type loop
         New_Index := (((I - Buffer'First) + Distance) mod Buffer'Length) + Workspace'First;
         Workspace(New_Index) := Buffer(I);
      end loop;
      Buffer := Workspace;
   end Rotate_Right;


   procedure Reverse_Buffer(Buffer : in out Buffer_Type) is
      Temp : Character;
   begin
      for I in 1 .. Buffer'Length / 2 loop
         Temp := Buffer(Buffer'First + (I - 1));
         Buffer(Buffer'First + (I - 1))  := Buffer(Buffer'Last - (I - 1));
         Buffer(Buffer'Last - (I - 1)) := Temp;
      end loop;
   end Reverse_Buffer;


   function Count_Character(Buffer : in Buffer_Type; Ch : in Character) return Buffer_Count_Type is
      Count : Buffer_Count_Type := 0;
   begin
      for Index in Buffer_Index_Type loop
         if Buffer(Index) = Ch then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_Character;


   procedure Count_And_Erase_Character
     (Buffer : in out Buffer_Type;
      Ch     : in     Character;
      Count  :    out Buffer_Count_Type)
   is
   begin
      Count := 0;
      for Index in Buffer_Index_Type loop
         if Buffer(Index) = Ch then
            Count := Count + 1;
            Buffer(Index) := ' ';
         end if;
      end loop;
   end Count_And_Erase_Character;


   procedure Compact
     (Buffer          : in out Buffer_Type;
      Erase_Character : in     Character;
      Fill_Character  : in     Character;
      Valid           :    out Buffer_Count_Type)
   is
      Workspace : Buffer_Type := (others => ' ');
      Offset    : Buffer_Count_Type := 0;
   begin
      -- Take interesting characters from the original buffer and pack them into Workspace.
      for I in Buffer_Index_Type loop
         if Buffer(I) /= Erase_Character then
            Workspace(Workspace'First + Offset) := Buffer(I);
            Offset := Offset + 1;
         end if;
      end loop;

      Valid := Offset;

      -- Install appropriate number of fill characters.
      for I in Buffer_Count_Type range Offset + 1 .. Buffer_Index_Type'Last loop
         Workspace(Workspace'First + I - 1) := Fill_Character;
      end loop;
      Buffer := Workspace;
   end Compact;


   procedure Copy_Into(Buffer : out Buffer_Type; Source : in  String) is
      Characters_To_Copy : Buffer_Count_Type := Maximum_Buffer_Size;
   begin
      Buffer := (others => ' ');  -- Initialize to all blanks
      if Source'Length < Characters_To_Copy then
         Characters_To_Copy := Source'Length;
      end if;
      for Index in 1 .. Characters_To_Copy loop
         Buffer(Index) := Source(Source'First + (Index - 1));
      end loop;
   end Copy_Into;


   procedure Copy_Onto
     (Buffer : in out Buffer_Type;
      Source : in     String;
      Point  : in     Buffer_Index_Type;
      Length : in     Buffer_Count_Type)
   is
      Characters_To_Copy : Buffer_Count_Type;
   begin
      Characters_To_Copy := Length;
      if Source'Length < Characters_To_Copy then
         Characters_To_Copy := Source'Length;
      end if;
      if Point + (Characters_To_Copy - 1) > Buffer'Last then
         Characters_To_Copy := (Buffer'Last - Point) + 1;
      end if;
      for I in 1 .. Characters_To_Copy loop
         Buffer(Point + (I - 1)) := Source(Source'First + (I - 1));
      end loop;
   end Copy_Onto;


   procedure Copy_From
     (Buffer      : in     Buffer_Type;
      Destination :    out String;
      Point       : in     Buffer_Index_Type;
      Length      : in     Buffer_Count_Type)
   is
      Characters_To_Copy : Buffer_Count_Type;
   begin
      Characters_To_Copy := Length;
      Destination := (others=> ' ');
      if Buffer'Length - Point < Characters_To_Copy then
         Characters_To_Copy := Buffer'Length - Point;
      end if;
      if Characters_To_Copy > Destination'Length then
         Characters_To_Copy := Destination'Length;
      end if;
      for I in 1 .. Characters_To_Copy loop
         Destination(Destination'First + (I - 1)) := Buffer(Point + (I - 1));
      end loop;
   end Copy_From;

   -- EXERCISES
   ------------

   procedure Rotate_Left(Buffer : in out Buffer_Type; Distance : in Buffer_Count_Type) is
      Temp : Character;
   begin
      for I in 1 .. Distance loop
         Temp := Buffer(Buffer'First);
         Buffer(Buffer'First .. Buffer'Last - 1) := Buffer(Buffer'First + 1 .. Buffer'Last);
         Buffer(Buffer'Last) := Temp;
      end loop;
   end Rotate_Left;


   function Count_Substrings(Buffer : in Buffer_Type; Search : in String) return Buffer_Count_Type is
      Result    : Buffer_Count_Type := 0;
      Positions : Buffer_Index_Type;
      Matched   : Boolean;
   begin
      if Search'Length > 0 and Buffer'Length >= Search'Length then
         Positions := (Buffer'Length - Search'Length) + 1;
         for I in 1 .. Positions loop
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


end Buffers2;
