
-- This is part of the solution to Lab #3 in CIS-2730 at Vermont Technical College (Spring 2022)
package body Clock_Times is

   function Advance(Current : in Clock_Time; Additional_Seconds : in Natural) return Clock_Time is
      Result : Clock_Time := Current;
      Overflow_Minutes : Natural;
      Overflow_Hours   : Natural;
   begin
      Overflow_Minutes := (Result.Seconds + Additional_Seconds) / 60;
      Result.Seconds   := (Result.Seconds + Additional_Seconds) rem 60;

      Overflow_Hours := (Result.Minutes + Overflow_Minutes) / 60;
      Result.Minutes := (Result.Minutes + Overflow_Minutes) rem 60;

      Result.Hours := (Result.Hours + Overflow_Hours) rem 24;
      return Result;
   end Advance;


   function Comes_Before(Left, Right : in Clock_Time) return Boolean is
   begin
      if Left.Hours < Right.Hours then return True; end if;
      if Left.Hours > Right.Hours then return False; end if;

      -- Otherwise, the hours are equal.
      if Left.Minutes < Right.Minutes then return True; end if;
      if Left.Minutes > Right.Minutes then return False; end if;

      -- Otherwise, the hours and the minutes are equal.
      if Left.Seconds < Right.Seconds then return True; end if;
      return False;
   end Comes_Before;


   function Difference(Left, Right : in Clock_Time) return Integer is
      Hours_Difference   : Integer;
      Minutes_Difference : Integer;
      Seconds_DIfference : Integer;
   begin
      Hours_Difference   := Left.Hours   - Right.Hours;
      Minutes_Difference := Left.Minutes - Right.Minutes;
      Seconds_Difference := Left.Seconds - Right.Seconds;
      return 3600*Hours_Difference + 60*Minutes_Difference + Seconds_Difference;
   end Difference;

end Clock_Times;
