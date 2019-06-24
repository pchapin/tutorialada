
separate(Checksum)
procedure Transmit_Data(Input : IN Block; Result : OUT Block) is
   Temp : Channel.Octet;
begin
   for I in Block'RANGE loop
      Temp := Input(I);
      Channel.Transceive(Temp);
      Result(I) := Temp;
   end loop;
end Transmit_Data;
