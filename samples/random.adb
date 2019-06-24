---------------------------------------------------------------------------
-- FILE          : random.adb
-- SUBJECT       : Implementation of a random number generator package.
---------------------------------------------------------------------------
with Interfaces; use Interfaces;

package body Random is

   function Make(Seed : Number_Type) return Generator is
      Result : Generator;
   begin
      Result.State := Unsigned_32(Seed);
      return Result;
   end Make;


   procedure Next(Gen : in out Generator; Number : out Number_Type) is
   begin
      Gen.State := (Gen.State * 1_103_514_245) + 12_345;
      Number := Number_Type( Shift_Right(Gen.State, 16) and 16#0000FFFF# );
   end Next;


   procedure Next
     (Gen       : in out Generator;
      Low, High : in     Number_Type;
      Number    :    out Number_Type) is

      Interval : Number_Type;
      Result   : Number_Type;
   begin
      if Low > High then raise Bad_Range; end if;

      Interval := High - Low + 1;  -- Could raise Constraint_Error here.
      Next(Gen, Result);
      Number := Low + (Result mod Interval);
   end Next;

end Random;
