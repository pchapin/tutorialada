
separate (Checksum)

procedure Transmit_Data (Input : Block; Result : out Block) is
   Temp : Channel.Octet;
begin
   for I in Block'Range loop
      Temp := Input (I);
      Channel.Transceive (Temp);
      Result (I) := Temp;
   end loop;
end Transmit_Data;
