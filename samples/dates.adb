---------------------------------------------------------------------------
-- FILE    : dates.adb
-- SUBJECT : Body of a package providing calendar dates.
--
---------------------------------------------------------------------------
pragma SPARK_Mode(On);

package body Dates is

   function Maximum_Date return Date is
     ((Day => 31, Month => Month_Type'Last, Year => Year_Type'Last));

   function Minimum_Date return Date is
     ((Day =>  1, Month => Month_Type'First, Year => Year_Type'First));

   -- Return True if the given year is a leap year.
   -- The method used here is correct for all years in the range of Year_Type.
   --
   function Is_Leap_Year(Year : in Year_Type) return Boolean is
      Result : Boolean := False;
   begin
      if Year mod 4 = 0 then
         Result := True;
      end if;
      return Result;
   end Is_Leap_Year;


   -- Return the length in days of the given month. The effect of leap years is considered.
   function Get_Month_Length(Year : in Year_Type; Month : in Month_Type) return Day_Type is
      Month_Length : constant array(Month_Type) of Day_Type :=
        (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

      Length : Day_Type;
   begin
      if Is_Leap_Year(Year) and Month = 2 then
         Length := 29;
      else
         Length := Month_Length(Month);
      end if;
      return Length;
   end Get_Month_Length;


   -- Return True if the given date is a valid date.
   function Is_Valid_Date(Year : Year_Type; Month : Month_Type; Day : Day_Type) return Boolean is
      Result : Boolean := True;
   begin
      if Day > Get_Month_Length(Year, Month) then
         Result := False;
      end if;
      return Result;
   end Is_Valid_Date;


   -- Advance the given date ahead by one day.
   procedure One_Day_Forward(Current_Date : in out Date) is
   begin
      if Current_Date.Day < Get_Month_Length(Current_Date.Year, Current_Date.Month) then
         Current_Date.Day := Current_Date.Day + 1;
      else
         if Current_Date.Month /= Month_Type'Last then
            Current_Date.Day   := 1;
            Current_Date.Month := Current_Date.Month + 1;
         else
            Current_Date.Day   := 1;
            Current_Date.Month := 1;
            Current_Date.Year  := Current_Date.Year + 1;
         end if;
      end if;
   end One_Day_Forward;


   -- Backs up the given date by one day.
   procedure One_Day_Backward(Current_Date : in out Date) is
   begin
      if Current_Date.Day /= 1 then
         Current_Date.Day := Current_Date.Day - 1;
      else
         if Current_Date.Month /= Month_Type'First then
            Current_Date.Month := Current_Date.Month - 1;
            Current_Date.Day   := Get_Month_Length(Current_Date.Year, Current_Date.Month);
         else
            Current_Date.Year  := Current_Date.Year - 1;
            Current_Date.Month := Month_Type'Last;
            Current_Date.Day   := 31;
         end if;
      end if;
   end One_Day_Backward;


   function "<"(Past : Date; Future : Date) return Boolean is
      Result : Boolean := False;
   begin
      if Past.Year < Future.Year then
         Result := True;
      elsif Past.Year = Future.Year then
         if Past.Month < Future.Month then
            Result := True;
         elsif Past.Month = Future.Month then
            if Past.Day < Future.Day then
               Result := True;
            end if;
         end if;
      end if;
      return Result;
   end "<";


   function "<"(Past : Time; Future : Time) return Boolean is
      Result : Boolean := False;
   begin
      if Past.Hour < Future.Hour then
         Result := True;
      elsif Past.Hour = Future.Hour then
         if Past.Minute < Future.Minute then
            Result := True;
         elsif Past.Minute = Future.Minute then
            if Past.Second < Future.Second then
               Result := True;
            end if;
         end if;
      end if;
      return Result;
   end "<";

   ----------------------
   -- Visible Subprograms
   ----------------------

   procedure Create_Date
     (Year     : in Year_Type;
      Month    : in Month_Type;
      Day      : in Day_Type;
      New_Date : out Date;
      Valid    : out Boolean) is

      Default_Date : Date;
   begin
      if Is_Valid_Date(Year, Month, Day) then
         New_Date.Year  := Year;
         New_Date.Month := Month;
         New_Date.Day   := Day;
         Valid          := True;
      else
         -- Use the default date if the given date is invalid.
         New_Date       := Default_Date;
         Valid          := False;
      end if;
   end Create_Date;


   function Create_Time
     (Hour   : in Hour_Type;
      Minute : in Minute_Type;
      Second : in Second_Type) return Time is
   begin
      return Time'(Hour, Minute, Second);
   end Create_Time;


   function Create_Datetime(Date_Part : in Date; Time_Part : in Time) return Datetime is
   begin
      return (Date_Part, Time_Part);
   end Create_Datetime;


   procedure Advance(Current_Date : in out Date; By : in Day_Advance_Type; Valid : out Boolean) is
      Steps : Day_Advance_Type;
   begin
      Valid := True;
      if By >= 0 then
         Steps := By;
         for I in 1 .. Steps loop
            if Current_Date = Maximum_Date then
               Valid := False;
               exit;
            end if;
            One_Day_Forward(Current_Date);
         end loop;
      else
         Steps := -By;
         for I in 1 .. Steps loop
            if Current_Date = Minimum_Date then
               Valid := False;
               exit;
            end if;
            One_Day_Backward(Current_Date);
         end loop;
      end if;
   end Advance;


   function "<"(Past : Datetime; Future : Datetime) return Boolean is
      Result : Boolean := False;
   begin
      if Past.Date_Part < Future.Date_Part then
         Result := True;
      elsif Past.Date_Part = Future.Date_Part then
         if Past.Time_Part < Future.Time_Part then
            Result := True;
         end if;
      end if;
      return Result;
   end "<";

end Dates;
