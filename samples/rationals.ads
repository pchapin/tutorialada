---------------------------------------------------------------------------
-- FILE          : rationals.ads
-- SUBJECT       : Specification of a rational number handling package.
--
-- Note that it is common to give packages containing a single private type a name that is
-- the plural form of the name used for the private type. Thus package Rationals contains
-- a type Rational. This convention is not universal, but it is common.
---------------------------------------------------------------------------

generic
   type Numeric_Type is new Integer;
package Rationals is

   -- The primary abstract type provided by this package.
   type Rational is private;

   -- Constructor.
   function Make(Numerator, Denominator : Numeric_Type) return Rational;

   -- Accessor functions.
   function Get_Numerator(R : Rational) return Numeric_Type;
   function Get_Denominator(R : Rational) return Numeric_Type;

   -- Mathematical operations.
   function "+"(Left, Right : Rational) return Rational;
   function "-"(Left, Right : Rational) return Rational;
   function "*"(Left, Right : Rational) return Rational;
   function "/"(Left, Right : Rational) return Rational;

private

   type Rational is
      record
         Numerator   : Numeric_Type;
         Denominator : Numeric_Type;
      end record;

end Rationals;
