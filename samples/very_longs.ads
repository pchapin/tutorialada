pragma SPARK_Mode(On);

-- This package provides arbitrary precision *unsigned* integers. It uses a discriminated type
-- to represent the integers, where the discriminate specifies the number of 8-bit "digits" in
-- the integer. In general only integers of the same size are allowed to interact and operations
-- follow modular arithmetic. These restrictions simplify the implementation and yet are still
-- fine for applications where large, consistently sized, unsigned integers are needed.
--
package Very_Longs is

   -- Here "Digit" means a base 256 digit (8 bits).

   -- At most only 64K "digits" are supported.
   Maximum_Length : constant := 2**16;

   type    Digit_Count_Type is new Natural range 0 .. Maximum_Length;
   subtype Digit_Index_Type is Digit_Count_Type range 1 .. Maximum_Length;

   -- Numbers must have at least one digit.
   type Very_Long(Length : Digit_Index_Type) is private;

   ---------------
   -- Constructors
   ---------------

   -- The precondition makes the assumption that type Natural is 32 bits.
   function Make_From_Natural
     (Number : Natural; Length : Digit_Index_Type) return Very_Long
     with
       Pre => (if Length < 4 then Number < 256**Natural(Length)),
       Post => Make_From_Natural'Result.Length = Length;

   subtype Hex_Digit_Type is Character
     with Static_Predicate => Hex_Digit_Type in '0' .. '9' | 'A' .. 'F' | 'a' .. 'f';

   function Make_From_Hex_String(Number_String : String) return Very_Long
     with
       Pre =>
         Number_String'Length > 0 and
         Number_String'Length <= 2*Maximum_Length and
         Number_String'Length mod 2 = 0 and
         (for all I in Number_String'Range => Number_String(I) in Hex_Digit_Type),
       Post =>
         Make_From_Hex_String'Result.Length = Number_String'Length/2;

   -----------------------
   -- Relational Operators
   -- Only Very_Longs of equal size can be compared.
   -----------------------

   function "<"(L, R : Very_Long) return Boolean
     with Pre => L.Length = R.Length;

   function "<="(L, R : Very_Long) return Boolean
     with Pre => L.Length = R.Length;

   function ">"(L, R : Very_Long) return Boolean
     with Pre => L.Length = R.Length;

   function ">="(L, R : Very_Long) return Boolean
     with Pre => L.Length = R.Length;

   -- Returns True if Number is zero.
   function Is_Zero(Number : Very_Long) return Boolean;

   -- Returns the number of significant digits in Number.
   -- That is, does not count leading zeros. Will return a count of zero for a value of zero.
   function Number_Of_Digits(Number : Very_Long) return Digit_Count_Type
     with Post => Number_Of_Digits'Result <= Number.Length;

   -----------------------
   -- Arithmetic Operators
   -----------------------
   -- The postconditions on this subprograms only makes assertions on the relative sizes of
   -- the integers. No attempt to made to specify the mathematical operations themselves. The
   -- correctness of the functionality is left by this package for testing to verify.

   -- Modular addition (modulo 256**Length).
   function ModAdd(L, R : Very_Long) return Very_Long
     with
       Pre  => L.Length = R.Length,
       Post => ModAdd'Result.Length = L.Length;

   -- Modular subtraction (modulo 256**Length).
   function ModSubtract (L, R : Very_Long) return Very_Long
     with
       Pre  => L.Length = R.Length,
       Post => ModSubtract'Result.Length = L.Length;

   -- Modular multiplication (modulo 256**Length).
   function ModMultiply (L, R : Very_Long) return Very_Long
     with
       Pre  => L.Length = R.Length,
       Post => ModMultiply'Result.Length = L.Length;

   -- Ordinary multiplication.
   function "*"(L, R : Very_Long) return Very_Long
     with
       Pre => L.Length + R.Length <= Maximum_Length,
       Post => "*"'Result.Length = L.Length + R.Length;

   -- Division returns quotient and remainder.
   -- Currently the implementation doesn't handle divisors of only one digit. It uses Knuth's
   -- "Algorithm D" which requires a supplementary algorithm for handling single digit divisors.
   procedure Divide
     (Dividend  : in  Very_Long;
      Divisor   : in  Very_Long;
      Quotient  : out Very_Long;
      Remainder : out Very_Long)
     with
       Depends =>
         (Quotient  =>+ (Dividend, Divisor),
          Remainder =>+ (Dividend, Divisor)),
       Pre =>
         (not Is_Zero(Divisor))               and
         (Number_Of_Digits(Divisor) > 1)      and
         (Divisor.Length  = Remainder.Length) and
         (Dividend.Length = Quotient.Length ) and
         (Dividend.Length = 2*Divisor.Length) and
         (Dividend.Length < Maximum_Length);

private
   type Octet is mod 2**8;
   type Double_Octet is mod 2**16;

   type Digits_Array_Type is array(Digit_Index_Type range <>) of Octet;

   -- The bytes are stored in little endian order.
   type Very_Long(Length : Digit_Index_Type) is
      record
         Long_Digits : Digits_Array_Type (1 .. Length);
      end record;

   function Is_Zero(Number : Very_Long) return Boolean is
     (for all I in Number.Long_Digits'Range => Number.Long_Digits(I) = 0);

end Very_Longs;
