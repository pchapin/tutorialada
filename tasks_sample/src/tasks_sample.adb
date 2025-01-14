
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Task_Demo;

use Ada.Text_IO;
use Ada.Integer_Text_IO;

procedure Tasks_Sample is
   Number   : Natural;
   Is_Prime : Boolean;
begin
   Put ("Enter an integer: ");
   Get (Number);

   Task_Demo.Check_Prime (N => Number, Result => Is_Prime);
   if Is_Prime then
      Put_Line ("It is prime.");
   else
      Put_Line ("It is not prime.");
   end if;

end Tasks_Sample;
