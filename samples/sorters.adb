---------------------------------------------------------------------------
-- FILE    : sorters.adb
-- SUBJECT : Body of a package providing various sorting procedures
--
---------------------------------------------------------------------------

package body Sorters is

   procedure Bubble_Sort(Values : in out Array_Type) is

      procedure Single_Pass(Limit : Index_Type)
        with
          Pre => Limit < Values'Last,
          Post => (for all K in Values'First .. Limit => (Values(K) <= Values(Limit + 1)))
      is
         Array_Item : Integer;
      begin
         for J in Values'First .. Limit loop
            if Values(J + 1) < Values(J) then
               Array_Item := Values(J);
               Values(J) := Values(J + 1);
               Values(J + 1) := Array_Item;
            end if;
         end loop;
      end Single_Pass;

   begin
      for I in reverse Values'First .. Values'Last - 1 loop
         Single_Pass(Limit => I);
      end loop;
   end Bubble_Sort;


   procedure Selection_Sort(Values : in out Array_Type; Limit  : in Index_Type) is

      -- Swaps the values of two components in the array Values.
      procedure Swap(X : in Index_Type; Y : in Index_Type) is
         Temp : Integer;
      begin
         Temp       := Values (X);
         Values (X) := Values (Y);
         Values (Y) := Temp;
      end Swap;

      -- Finds the smallest element in the remaining unsorted data.
      function Index_Of_Minimum(Starting_At : in Index_Type) return Index_Type
        with Post =>
          Index_Of_Minimum'Result >= Starting_At and then
          (for all J in Starting_At .. Index_Type'Last =>
             Values(Index_Of_Minimum'Result) <= Values(J))
      is
         Min : Index_Type;
      begin
         Min := Starting_At;
         for Index in Index_Type range Starting_At + 1 .. Limit loop
            if Values (Index) < Values (Min) then
               Min := Index;
            end if;
         end loop;
         return Min;
      end Index_Of_Minimum;

   begin -- Selection_Sort
      for Current in Index_Type range Values'First .. Limit loop
         Swap(Current, Index_Of_Minimum(Starting_At => Current));
      end loop;
   end Selection_Sort;


   procedure Merge_Sort(Values : in out Array_Type) is

      subtype Distance_Type is Count_Type range 1 .. Count_Type'Last;
      Region_Counter : Count_Type;
      Region_Size    : Count_Type;

      -- Merge the sorted, contiguous subsequences of Values from A_Position to A_Position +
      -- (A_Distance - 1) and from B_Position to B_Position + (B_Distance - 1)
      procedure Merge
        (A_Position : in     Index_Type;
         A_Distance : in     Distance_Type;
         B_Position : in     Index_Type;
         B_Distance : in     Distance_Type)
        with
          Pre => B_Position = A_Position + A_Distance and
                 B_Position + (B_Distance - 1) <= Values'Last
      is
         Workspace : Array_Type;      -- Temporary space to hold merged result.
         A_Side    : Index_Type;      -- Index of current element in the first region to merge.
         B_Side    : Index_Type;      -- Index of current element in the second region to merge.
         Merged    : Index_Type;      -- Index of next empty slot in merged result.
         A_Active  : Boolean := True; -- True when there are elements left in the first region.
         B_Active  : Boolean := True; -- True when there are elements left in the second region.

         procedure Take_A is
         begin
            Workspace(Merged) := Values(A_Side);

            if Merged /= Workspace'Last then
               Merged := Merged + 1;
            end if;

            if A_Side = A_Position + (A_Distance - 1) then
               A_Active := False;
            else
               A_Side := A_Side + 1;
            end if;
         end Take_A;

         procedure Take_B is
         begin
            Workspace(Merged) := Values(B_Side);

            if Merged /= Workspace'Last then
               Merged := Merged + 1;
            end if;

            if B_Side = B_Position + (B_Distance - 1) then
               B_Active := False;
            else
               B_Side := B_Side + 1;
            end if;
         end Take_B;

      begin -- Merge
         A_Side    := A_Position;
         B_Side    := B_Position;
         Merged    := Workspace'First;
         Workspace := Array_Type'(others => 0);

         -- Compute merged result.
         while A_Active or B_Active loop
            if not A_Active then
               Take_B;
            elsif not B_Active then
               Take_A;
            elsif Values(A_Side) < Values(B_Side) then
               Take_A;
            else
               Take_B;
            end if;
         end loop;

         -- Copy merged result back into Values.
         for I in Workspace'First .. (Workspace'First + ((A_Distance + B_Distance) - 1)) loop
            Values(A_Position + (I - Workspace'First)) := Workspace(I);
         end loop;
      end Merge;

   begin -- Merge_Sort
      Region_Counter := Values'Length / 2;

      while Region_Counter > 0 loop
         Region_Size := Values'Length / Region_Counter;
         for I in Count_Type range 1 .. Region_Counter loop
            Merge
              (A_Position =>  Values'First + Region_Size * (I - 1),
               A_Distance =>  Region_Size / 2,
               B_Position => (Values'First + Region_Size * (I - 1)) + Region_Size / 2,
               B_Distance =>  Region_Size - (Region_Size / 2));
         end loop;
         Region_Counter := Region_Counter / 2;
      end loop;
   end Merge_Sort;

end Sorters;
