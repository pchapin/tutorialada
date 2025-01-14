
separate (Checksum)

procedure Generate_Data (Holding_Tank : in out Block) is
begin
   for I in Block'Range loop
      if I mod 2 = 0 then
         Holding_Tank (I) := 16#AA#;
      else
         Holding_Tank (I) := 16#55#;
      end if;
   end loop;
end Generate_Data;
