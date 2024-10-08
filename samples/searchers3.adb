package body Searchers3
  with SPARK_Mode => On
is
   procedure Binary_Search
     (Search_Item : in  Integer;
      Items       : in  Array_Type;
      Found       : out Boolean;
      Result      : out Index_Type)
   is
      Low_Index  : Index_Type := Items'First;
      Mid_Index  : Index_Type;
      High_Index : Index_Type := Items'Last;
   begin
      Found  := False;
      Result := Items'First;  -- Initialize Result to "not found" case.

      -- If the item is out of range, it is not found.
      if Search_Item < Items(Low_Index) or Items(High_Index) < Search_Item then
         return;
      end if;

      loop
         Mid_Index := (Low_Index + High_Index) / 2;
         if Search_Item = Items(Mid_Index) then
            Found  := True;
            Result := Mid_Index;
            return;
         end if;

         exit when Low_Index = High_Index;

         pragma Loop_Invariant(Mid_Index in Low_Index .. High_Index - 1);
         pragma Loop_Invariant(Items(Low_Index) <= Search_Item);
         pragma Loop_Invariant(Search_Item <= Items(High_Index));
         pragma Loop_Variant(Decreases => High_Index - Low_Index);

         if Items(Mid_Index) < Search_Item then
            if Search_Item < Items(Mid_Index + 1) then
               return;
            end if;
            Low_Index := Mid_Index + 1;
         else
            High_Index := Mid_Index;
         end if;

      end loop;
   end Binary_Search;

end Searchers3;
