pragma SPARK_Mode(On);

package body Calendar_Dates is

   function Is_Leap(Year : in Year_Type) return Boolean is
   begin
      if Year rem 400 = 0 then return True;  end if;
      if Year rem 100 = 0 then return False; end if;
      if Year rem   4 = 0 then return True;  end if;
      return False;
   end Is_Leap;


   function Month_Length(Current : in Date) return Day_Type is
      Length : Day_Type;
   begin
      case Current.Month is
         when  1 => Length := 31;
         when  2 => Length := 28;
         when  3 => Length := 31;
         when  4 => Length := 30;
         when  5 => Length := 31;
         when  6 => Length := 30;
         when  7 => Length := 31;
         when  8 => Length := 31;
         when  9 => Length := 30;
         when 10 => Length := 31;
         when 11 => Length := 30;
         when 12 => Length := 31;
      end case;

      -- Deal with Feburary as a special case.
      if Current.Month = 2 and Is_Leap(Current.Year) then
         Length := 29;
      end if;
      return Length;
   end Month_Length;


   function Next(Now : in Date) return Date is
      Result : Date := Now;
   begin
      if Month_Length(Now) /= Now.Day then
         Result.Day := Result.Day + 1;
      else
         if Result.Month /= 12 then
            Result := (Year => Result.Year, Month => Result.Month + 1, Day => 1);
         else
            -- This raises Constraint_Error if an attempt is made to go beyond Year_Type'Last.
            Result := (Year => Result.Year + 1, Month => 1, Day => 1);
         end if;
      end if;
      return Result;
   end Next;


   function Previous(Now : in Date) return Date is
   begin
      -- TODO: Finish me!
      return Now;
   end Previous;


   function Comes_Before(Left : in Date; Right : in Date) return Boolean is
   begin
      -- TODO: Finish me!
      return True;
   end Comes_Before;

end Calendar_Dates;
