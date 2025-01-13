with Ada.Text_IO;
with Calendar_Dates; use Calendar_Dates;

procedure Calendar_Dates_Demo is
   Today    : Date := (Year => 2022, Month => 2, Day => 1);
   Tomorrow : Date;
   Yesterday: Date;
begin
   Tomorrow := Next(Today);
   Yesterday := Previous(Today);

   if Today = Tomorrow then
      Ada.Text_IO.Put_Line("This is a very strange world.");
   end if;

   if Comes_Before(Yesterday, Today) then
      Ada.Text_IO.Put_Line("That makes sense!");
   end if;

end Calendar_Dates_Demo;
