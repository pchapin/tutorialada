---------------------------------------------------------------------------
-- FILE   : rationals.adb
-- SUBJECT: Implementation of a rational number handling package.
--
---------------------------------------------------------------------------

package body Rationals is

   -----------------------
   -- Internal Subprograms
   -----------------------

   procedure Reduce(R : in out Rational) is

      -- Euclid's algorithm for finding the greatest common divisor.
      function GCD(X, Y : Numeric_Type) return Numeric_Type is
         M         : Numeric_Type := X;
         N         : Numeric_Type := Y;
         Remainder : Numeric_Type;
      begin
         while N /= 0 loop
            Remainder := M mod N;
            M := N;
            N := Remainder;
         end loop;
         return M;
      end GCD;

      Divisor : Numeric_Type;

   begin -- Reduce
      if R.Numerator < 0 then
         Divisor := GCD(-R.Numerator, R.Denominator);
      else
         Divisor := GCD( R.Numerator, R.Denominator);
      end if;
      R.Numerator := R.Numerator / Divisor;
      R.Denominator := R.Denominator / Divisor;
   end Reduce;

   --------------
   -- Constructor
   --------------

   function Make(Numerator, Denominator : Numeric_Type) return Rational is
      Result : Rational := (Numerator, Denominator);
   begin
      if Result.Denominator < 0 then
         Result.Numerator := -Result.Numerator;
         Result.Denominator := -Result.Denominator;
      end if;
      Reduce(Result);
      return Result;
   end Make;

   ---------------------
   -- Accessor Functions
   ---------------------

   function Get_Numerator(R : Rational) return Numeric_Type is
   begin
      return R.Numerator;
   end Get_Numerator;


   function Get_Denominator(R : Rational) return Numeric_Type is
   begin
      return R.Denominator;
   end Get_Denominator;

   --------------------------
   -- Mathematical Operations
   --------------------------

   function "+"(Left, Right : Rational) return Rational is
      Result : Rational;
      Common : constant Numeric_Type := Left.Denominator * Right.Denominator;
   begin
      Result.Numerator :=
         (Left.Numerator * Right.Denominator) + (Right.Numerator * Left.Denominator);
      Result.Denominator := Common;
      Reduce(Result);
      return Result;
   end "+";


   function "-"(Left, Right : Rational) return Rational is
      Result : Rational;
      Common : constant Numeric_Type := Left.Denominator * Right.Denominator;
   begin
      Result.Numerator :=
         (Left.Numerator * Right.Denominator) - (Right.Numerator * Left.Denominator);
      Result.Denominator := Common;
      Reduce(Result);
      return Result;
   end "-";


   function "*"(Left, Right : Rational) return Rational is
      Result : Rational;
   begin
      Result.Numerator := Left.Numerator * Right.Numerator;
      Result.Denominator := Left.Denominator * Right.Denominator;
      Reduce(Result);
      return Result;
   end "*";


   function "/"(Left, Right : Rational) return Rational is
      Result : Rational;
   begin
      Result.Numerator := Left.Numerator * Right.Denominator;
      Result.Denominator := Left.Denominator * Right.Numerator;
      Reduce(Result);
      return Result;
   end "/";

end Rationals;
