with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Clock_Times;

use Ada.Text_IO;
use Ada.Integer_Text_IO;
use Clock_Times;

-- This is part of the solution to Lab #3 in CIS-2730 at Vermont Technical College (Spring 2022)
procedure Clock_Times_Demo is
   Start_Time : constant Clock_Time := (Hours => 23, Minutes => 0, Seconds => 0);
   Stop_Time  : Clock_Time;

   procedure Put_Time(Moment : in Clock_Time) is

      -- Is there a standard way to do zero fill? I can't find one.
      procedure Put_With_Fill(Number : in Natural) is
      begin
         if Number < 10 then
            Put('0');
            Put(Number, 1);
         else
            Put(Number, 2);
         end if;
      end Put_With_Fill;

   begin
      Put_With_Fill(Moment.Hours);
      Put(':');
      Put_With_Fill(Moment.Minutes);
      Put(':');
      Put_With_Fill(Moment.Seconds);
   end Put_Time;

begin
   Stop_Time := Advance(Start_Time, 7200);
   Put("Start_Time = "); Put_Time(Start_Time); New_Line;
   Put("Stop_Time  = "); Put_Time(Stop_Time ); New_Line;

   if Comes_Before(Stop_Time, Start_Time) then
      Put_Line("Stop_Time comes before Start_Time (this is expected)");
   else
      Put_Line("Start_Time comes before Stop_Time (this is NOT expected)");
   end if;

   Put("The number of seconds difference is ");
   Put(Difference(Start_Time, Stop_Time), 0);
   New_Line;
end Clock_Times_Demo;
