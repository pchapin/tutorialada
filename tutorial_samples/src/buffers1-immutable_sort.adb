pragma SPARK_Mode(On);

procedure Buffers1.Immutable_Sort(Input : in Buffer_Type; Output : out Buffer_Type) is
   Copied : Buffer_Count_Type := 0;
   Point  : Buffer_Index_Type;

   function Find_Insertion_Point(Ch : in Character) return Buffer_Index_Type
      with
         Pre  => (for all I in 1 .. Copied - 1 => Output(I) < Output(I + 1)),
         Post =>
         (for all J in 1 .. Find_Insertion_Point'Result - 1 => Output(J) <= Ch) and
         (for all J in Find_Insertion_Point'Result .. Copied => Ch < Output(J))
   is
      Result : Buffer_Index_Type := 1;
   begin
      if Copied /= 0 then
         for I in 1 .. Copied loop
            pragma Loop_Invariant(for all J in 1 .. I - 1 => Output(J) <= Ch);

            if Ch < Output(I) then
               Result := I;
               exit;
            end if;

            -- Handle the case where the new item is appended.
            if I = Copied then
               Result := I;
            end if;
         end loop;
      end if;
      return Result;
   end Find_Insertion_Point;

begin
   Output := (others => ' ');

   for I in Input'Range loop
      --pragma Loop_Invariant(Copied < Buffer_Count_Type'Last);
      --pragma Loop_Invariant(for all I in 1 .. Copied =>
      --                       (for some J in Input'Range => Output(I) = Input(J)));

      Point := Find_Insertion_Point(Input(I));
      Output(Point + 1 .. Output'Last) := Output(Point .. Output'Last - 1);
      Output(Point) := Input(I);
      pragma Loop_Invariant(Copied = I - 1);
      Copied := Copied + 1;
   end loop;
end Buffers1.Immutable_Sort;