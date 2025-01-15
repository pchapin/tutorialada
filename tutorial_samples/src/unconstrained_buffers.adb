pragma SPARK_Mode(On);

package body Unconstrained_Buffers is

   procedure Fill(Buffer : out Buffer_Type; Fill_Character : in Character) is
   begin
      Buffer := (others => Fill_Character);
   end Fill;

   procedure Rotate_Right(Buffer : in out Buffer_Type; Distance : in Buffer_Count_Type) is
      Workspace : Buffer_Type(Buffer'Range);
      New_Index : Buffer_Index_Type;

      Original_Buffer : constant Buffer_Type := Buffer
        with Ghost;
   begin
      Workspace := (others => ' ');
      for I in Buffer'Range loop
         pragma Loop_Invariant
           (for all J in Buffer'First .. I - 1 =>
              Workspace((((J - Buffer'First) + Distance) mod Buffer'Length) + Buffer'First) = Original_Buffer(J));

         New_Index := (((I - Buffer'First) + Distance) mod Buffer'Length) + Workspace'First;
         Workspace(New_Index) := Buffer(I);
      end loop;
      Buffer := Workspace;
   end Rotate_Right;

   procedure Reverse_Buffer(Buffer : in out Buffer_Type) is
      Workspace : Buffer_Type(Buffer'Range) := (others => ' ');
   begin
      for I in Buffer'Range loop
         pragma Loop_Invariant
           (for all J in Buffer'First .. I - 1 =>
              Workspace(J) = Buffer(Buffer'Last - (J - Buffer'First)));

         Workspace(I) := Buffer(Buffer'Last - (I - Buffer'First));
      end loop;
      Buffer := Workspace;
   end Reverse_Buffer;

   function Count_Character(Buffer : in Buffer_Type; Ch : in Character) return Buffer_Count_Type is
      Count : Buffer_Count_Type := 0;
   begin
      for Index in Buffer'Range loop
         pragma Loop_Invariant (Count < Index - Buffer'First + 1);

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
      Original_Buffer : constant Buffer_Type := Buffer
        with Ghost;
   begin
      Count := 0;
      for Index in Buffer'Range loop
         pragma Loop_Invariant(Count < Index - Buffer'First + 1);
         pragma Loop_Invariant
           (for all I in Buffer'First .. Index - 1 =>
              (if Original_Buffer(I) = Ch then Buffer(I) = ' '
                                          else Buffer(I) = Original_Buffer(I)));

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
      Workspace : Buffer_Type(Buffer'Range);
      Offset    : Buffer_Count_Type;
   begin
      Workspace := (others => ' ');
      Offset := 0;

      -- Take interesting characters from the original buffer and pack them into Workspace.
      for I in Buffer'Range loop
         pragma Loop_Invariant(Offset < I);
         pragma Loop_Invariant
           (for all J in Buffer'First .. Offset =>
              Workspace(J) /= Erase_Character);

         if Buffer(I) /= Erase_Character then
            Workspace(Workspace'First + Offset) := Buffer(I);
            Offset := Offset + 1;
         end if;
      end loop;

      Valid := Offset;

      -- Install appropriate number of fill characters.
      for I in Buffer_Count_Type range Offset + 1 .. Buffer'Last loop
         pragma Loop_Invariant
           (for all I in Buffer'First .. Buffer'First + Valid - 1 =>
              Workspace(I) /= Erase_Character);
         pragma Loop_Invariant
           (for all J in Offset + 1 .. I - 1 =>
              Workspace(J) = Fill_Character);

         Workspace(Workspace'First + I - 1) := Fill_Character;
      end loop;
      Buffer := Workspace;
   end Compact;

   procedure Copy_Into(Buffer : out Buffer_Type; Source : in  String) is
      Characters_To_Copy : constant Buffer_Count_Type :=
        Buffer_Count_Type'Min(Buffer'Length, Source'Length);
   begin
      Buffer := (others => ' ');  -- Initialize to all blanks
      for Index in Buffer_Count_Type range 1 .. Characters_To_Copy loop
         pragma Loop_Invariant
           (for all J in Buffer'First .. Index - 1 =>
              (Buffer(J) = (if J  - Buffer'First < Source'Length
                               then Source(Source'First + (J - Buffer'First))
                               else ' ')));

         Buffer(Index) := Source(Source'First + (Index - 1));
      end loop;
   end Copy_Into;

   procedure Copy_Onto
     (Buffer : in out Buffer_Type;
      Source : in     String;
      Point  : in     Buffer_Index_Type;
      Length : in     Buffer_Count_Type)
   is
      Minimum_Length  : Buffer_Count_Type;
      Original_Buffer : constant Buffer_Type := Buffer
        with Ghost;
   begin
      Minimum_Length := Buffer_Count_Type'Min(Length, Source'Length);
      for I in Buffer_Index_Type loop
         pragma Loop_Invariant
           (for all J in Buffer'First .. I - 1 =>
              Buffer(J) = (if J < Point
                             then Original_Buffer(J)
                             else (if J - Point < Minimum_Length
                                     then Source(Source'First + (J - Point))
                                     else Original_Buffer(J))));

         if I >= Point and I - Point < Minimum_Length then
            Buffer(I) := Source(Source'First + (I - Point));
         end if;
      end loop;
   end Copy_Onto;

   procedure Copy_From
     (Buffer      : in     Buffer_Type;
      Destination :    out String;
      Point       : in     Buffer_Index_Type;
      Length      : in     Buffer_Count_Type)
   is
      Minimum_Length : Buffer_Count_Type;
   begin
      Destination := (others => ' ');
      Minimum_Length := Buffer_Count_Type'Min(Length, Buffer'Last - Point + 1);
      Minimum_Length := Buffer_Count_Type'Min(Minimum_Length, Destination'Length);
      for I in Destination'Range loop
         pragma Loop_Invariant
           (for all J in Destination'First .. I - 1 =>
              Destination(J) = (if J - Destination'First < Minimum_Length
                                  then Buffer(Point + (J - Destination'First))
                                  else ' '));

         if I - Destination'First < Minimum_Length then
            Destination(I) := Buffer(Point + (I - Destination'First));
         end if;
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

end Unconstrained_Buffers;
