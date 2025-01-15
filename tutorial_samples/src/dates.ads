---------------------------------------------------------------------------
-- FILE    : dates.ads
-- SUBJECT : Package providing calendar dates.
--
-- Although the Ada standard library contains a package Ada.Calendar that would be more
-- appropriate to use than this package in most cases (because it is standard), this package
-- illustrates a number of useful techniques.
--
-- Exercise: Add a subprogram that computes the difference between two dates returning the
-- number of days apart the dates are. The subprogram should return its result using the signed
-- type Day_Advance_Type.
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

package Dates is
   type Year_Type   is range 2000 .. 2099;
   type Month_Type  is range 1 .. 12;
   type Day_Type    is range 1 .. 31;  -- Days can be in this range and yet still be illegal.

   type Hour_Type   is range 0 .. 23;
   type Minute_Type is range 0 .. 59;
   type Second_Type is range 0 .. 59;

   -- Represents the number of date values in the range between 2000-01-01 and 2099-12-31.
   subtype Day_Count_Type is Natural range 0 .. 36525;

   -- Represents the number of steps between two dates.
   subtype Day_Advance_Type is Integer range -(Day_Count_Type'Last - 1) .. (Day_Count_Type'Last - 1);

   type Date is private;
   type Time is private;
   type Datetime is private;

   -- Used to initialize Date objects with user specified values. Valid is set to True if the
   -- date is valid, otherwise it is set to False. Note that the subtypes themselves are not
   -- insufficient to ensure validity in this case.
   procedure Create_Date
     (Year     : in  Year_Type;
      Month    : in  Month_Type;
      Day      : in  Day_Type;
      New_Date : out Date;
      Valid    : out Boolean)
     with
       Post => (if Valid then Get_Year (New_Date) = Year and Get_Month (New_Date) = Month and Get_Day (New_Date) = Day);

   -- This subprogram can't fail so it doesn't need to return a Valid indicator. This also
   -- allows it to be a function rather than a procedure since it only returns one thing.
   function Create_Time
     (Hour   : in Hour_Type;
      Minute : in Minute_Type;
      Second : in Second_Type) return Time;

   -- This subprogram can't fail so it doesn't need to return a Valid indicator.
   function Create_Datetime(Date_Part : in Date; Time_Part : in Time) return Datetime;

   -- Accessor primitive operations.
   function Get_Year(Current_Date  : Date) return Year_Type  with Inline;
   function Get_Month(Current_Date : Date) return Month_Type with Inline;
   function Get_Day(Current_Date   : Date) return Day_Type   with Inline;

   function Get_Hour(Current_Time   : Time) return Hour_Type   with Inline;
   function Get_Minute(Current_Time : Time) return Minute_Type with Inline;
   function Get_Second(Current_Time : Time) return Second_Type with Inline;

   function Get_Date(Current_Moment : Datetime) return Date with Inline;
   function Get_Time(Current_Moment : Datetime) return Time with Inline;

   -- Advances a date by the specified amount. It is permitted to advance by a negative amount.
   -- If an attempt is made to advance off the end of the valid range of dates, Valid is set to
   -- False and Current_Date is set to the last allowed date in the direction of advancement.
   procedure Advance(Current_Date : in out Date; By : in Day_Advance_Type; Valid : out Boolean);

   -- Returns True if Past comes before Future.
   function "<"(Past : Datetime; Future : Datetime) return Boolean;

private

   type Date is
      record
         Year  : Year_Type  := Year_Type'First;
         Month : Month_Type := 1;
         Day   : Day_Type   := 1;
      end record;

   type Time is
      record
         Hour   : Hour_Type   := 0;
         Minute : Minute_Type := 0;
         Second : Second_Type := 0;
      end record;

   type Datetime is
      record
         Date_Part : Date;
         Time_Part : Time;
      end record;

   --  Expression functions for the trivial accessors.
   function Get_Year  (Current_Date : Date) return Year_Type   is (Current_Date.Year);
   function Get_Month (Current_Date : Date) return Month_Type  is (Current_Date.Month);
   function Get_Day   (Current_Date : Date) return Day_Type    is (Current_Date.Day);
   function Get_Hour  (Current_Time : Time) return Hour_Type   is (Current_Time.Hour);
   function Get_Minute(Current_Time : Time) return Minute_Type is (Current_Time.Minute);
   function Get_Second(Current_Time : Time) return Second_Type is (Current_Time.Second);
   function Get_Date  (Current_Moment : Datetime) return Date  is (Current_Moment.Date_Part);
   function Get_Time  (Current_Moment : Datetime) return Time  is (Current_Moment.Time_Part);

end Dates;
