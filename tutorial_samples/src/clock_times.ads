
-- This is part of the solution to Lab #3 in CIS-2730 at Vermont Technical College (Spring 2022)
package Clock_Times is

   subtype Seconds_Type is Natural range 0 .. 59;
   subtype Minutes_Type is Natural range 0 .. 59;
   subtype Hours_Type   is Natural range 0 .. 23;

   type Clock_Time is
      record
         Hours   : Hours_Type;
         Minutes : Minutes_Type;
         Seconds : Seconds_Type;
      end record;

   -- It might be better to define a subtype that can hold the maximum advancement.
   function Advance(Current : in Clock_Time; Additional_Seconds : in Natural) return Clock_Time;

   function Comes_Before(Left, Right : in Clock_Time) return Boolean;

   -- It might be better to define a subtype bounded by the maximum difference.
   function Difference(Left, Right : in Clock_Time) return Integer;

end Clock_Times;
