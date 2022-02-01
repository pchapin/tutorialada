pragma SPARK_Mode(On);

package Calendar_Dates is

   subtype Month_Type is Integer range 1 .. 12;
   subtype Day_Type   is Integer range 1 .. 31;
   subtype Year_Type  is Integer range 1900 .. 2099;

   type Date is
      record
         Year  : Year_Type;
         Month : Month_Type;
         Day   : Day_Type;
      end record;

   -- Advance 'Now' by one day, returning the result.
   function Next(Now : in Date) return Date;

   -- Roll back 'Now' by one day, returning the result.
   function Previous(Now : in Date) return Date;

   -- Returns True if 'Left' comes before 'Right'
   function Comes_Before(Left : in Date; Right : in Date) return Boolean;

end Calendar_Dates;
