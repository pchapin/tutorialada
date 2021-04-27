
separate(Checksum)
procedure Transmit_Data(Input : in Block; Result : out Block) is
   Temp : Channel.Octet;
begin
   for I in Block'range loop
      Temp := Input(I);
      Channel.Transceive(Temp);
      Result(I) := Temp;
   end loop;
end Transmit_Data;
