pragma SPARK_Mode(On);

package body Very_Longs is


   Digit_Bits : constant := 8;

   ---------------------
   -- Helper Subprograms
   ---------------------

   -- We need to declare some functions for shifting. GNAT supplies these intrinsically for
   -- all modular types. We just need to declare them to use them. Here 'Count' is the number
   -- of bits to shift.

   function Shift_Left(Value : Octet; Count : Natural) return Octet
     with
       Global => null,
       Import,
       Convention => Intrinsic;


   function Shift_Right(Value : Octet; Count : Natural) return Octet
     with
       Global => null,
       Import,
       Convention => Intrinsic;


   -- We actually don't need this one.
   --function Shift_Left(Value : Double_Octet; Count : Natural) return Double_Octet
   --  with
   --    Global => null,
   --    Import,
   --    Convention => Intrinsic;


   function Shift_Right(Value : Double_Octet; Count : Natural) return Double_Octet
     with
       Global => null,
       Import,
       Convention => Intrinsic;


   function Get_Hex_Digit(Raw_Digit : Hex_Digit_Type) return Octet is
     (case Raw_Digit is
        when '0' .. '9' => (Character'Pos(Raw_Digit) - Character'Pos('0')),
        when 'A' .. 'F' => (Character'Pos(Raw_Digit) - Character'Pos('A')) + 10,
        when 'a' .. 'f' => (Character'Pos(Raw_Digit) - Character'Pos('a')) + 10);


   -- Similar to ModAdd except that final carry is written to Carry.
   procedure ModAdd_And_Carry
     (L, R : in Very_Long; Result : out Very_Long; Carry : out Double_Octet)
     with
       Depends => (Result =>+ (L, R), Carry => (L, R)),
       Pre  => L.Length = R.Length and Result.Length = L.Length
   is
      L_Digit : Double_Octet;
      R_Digit : Double_Octet;
      Sum     : Double_Octet;
   begin
      Result.Long_Digits := (others => 16#00#);
      Carry := 0;
      for I in L.Long_Digits'Range loop
         L_Digit := Double_Octet(L.Long_Digits(I));
         R_Digit := Double_Octet(R.Long_Digits(I));
         Sum     := L_Digit + R_Digit + Carry;
         Carry   := Shift_Right(Sum, Digit_Bits);
         Result.Long_Digits(I) := Octet(Sum and 16#00FF#);
      end loop;
   end ModAdd_And_Carry;


   -- Similar to ModSubtract except that final borrow is written to Borrow.
   procedure ModSubtract_And_Borrow
     (L, R : in Very_Long; Result : out Very_Long; Borrow : out Double_Octet)
     with
       Depends => (Result =>+ (L, R), Borrow => (L, R)),
       Pre  => L.Length = R.Length and Result.Length = L.Length
   is
      L_Digit    : Double_Octet;
      R_Digit    : Double_Octet;
      Difference : Double_Octet;
   begin
      Result.Long_Digits := (others => 16#00#);
      Borrow := 0;
      for I in L.Long_Digits'Range loop
         L_Digit    := Double_Octet(L.Long_Digits(I));
         R_Digit    := Double_Octet(R.Long_Digits(I));
         Difference := (L_Digit - R_Digit) - Borrow;
         if (Difference and 16#FF00#) /= 0 then
            Borrow := 1;
         else
            Borrow := 0;
         end if;
         Result.Long_Digits(I) := Octet(Difference and 16#00FF#);
      end loop;
   end ModSubtract_And_Borrow;


   ----------------------
   -- Visible Subprograms
   ----------------------

   -- Constructors
   ---------------

   function Make_From_Natural
     (Number : in Natural; Length : in Digit_Index_Type) return Very_Long is

      Result : Very_Long(Length);
      Temp   : Natural;
   begin
      pragma Assert(Result.Length = Length);
      Result.Long_Digits := (others => 16#00#);
      Temp := Number;
      for Index in Result.Long_Digits'Range loop
         Result.Long_Digits(Index) := Octet(Temp rem 256);
         Temp := Temp / 256;
      end loop;
      return Result;
   end Make_From_Natural;


   function Make_From_Hex_String(Number_String : String) return Very_Long is
      String_Index : Positive;
      Result       : Very_Long(Number_String'Length/2);
   begin
      Result.Long_Digits := (others => 16#00#);

      String_Index := Number_String'First;
      for Index in reverse Result.Long_Digits'Range loop
         pragma Loop_Invariant
           (String_Index = Number_String'First + Natural(2*(Result.Length - Index)));

         Result.Long_Digits(Index) :=
           16 * Get_Hex_Digit(Number_String(String_Index)) + Get_Hex_Digit(Number_String(String_Index + 1));

         if String_Index < Number_String'Last - 2 then
            String_Index := String_Index + 2;
         end if;
      end loop;
      return Result;
   end Make_From_Hex_String;


   -- Relational Operators
   -----------------------

   function "<"(L, R : Very_Long) return Boolean is
      Result : Boolean := False;  -- Use this value if they are equal.
   begin
      for I in reverse L.Long_Digits'Range loop
         if L.Long_Digits(I) < R.Long_Digits(I) then
            Result := True;
            exit;
         end if;
         if L.Long_Digits(I) > R.Long_Digits(I) then
            Result := False;
            exit;
         end if;
      end loop;

      return Result;
   end "<";


   function "<="(L, R : Very_Long) return Boolean is
   begin
      return (L < R) or (L = R);
   end "<=";


   function ">"(L, R : Very_Long) return Boolean is
   begin
      return not (L <= R);
   end ">";


   function ">="(L, R : Very_Long) return Boolean is
   begin
      return not (L < R);
   end ">=";


   function Number_Of_Digits(Number : Very_Long) return Digit_Count_Type is
      Result : Digit_Count_Type := 0;
   begin
      for I in reverse Number.Long_Digits'Range loop
         pragma Loop_Invariant(Result <= Number.Length);

         if Result = 0 and Number.Long_Digits(I) /= 0 then
            Result := I;
         end if;
      end loop;
      return Result;
   end Number_Of_Digits;


   -- Arithmetic Operators
   -----------------------

   function ModAdd(L, R : Very_Long) return Very_Long is
      Result  : Very_Long(L.Length);
      L_Digit : Double_Octet;
      R_Digit : Double_Octet;
      Sum     : Double_Octet;
      Carry   : Double_Octet;
   begin
      Result.Long_Digits := (others => 16#00#);
      Carry := 0;
      for I in L.Long_Digits'Range loop
         L_Digit := Double_Octet(L.Long_Digits(I));
         R_Digit := Double_Octet(R.Long_Digits(I));
         Sum     := L_Digit + R_Digit + Carry;
         Carry   := Shift_Right(Sum, Digit_Bits);
         Result.Long_Digits(I) := Octet(Sum and 16#00FF#);
      end loop;
      return Result;
   end ModAdd;


   function ModSubtract(L, R : Very_Long) return Very_Long is
      Result     : Very_Long(L.Length);
      L_Digit    : Double_Octet;
      R_Digit    : Double_Octet;
      Difference : Double_Octet;
      Borrow     : Double_Octet;
   begin
      Result.Long_Digits := (others => 16#00#);
      Borrow := 0;
      for I in L.Long_Digits'Range loop
         L_Digit    := Double_Octet(L.Long_Digits(I));
         R_Digit    := Double_Octet(R.Long_Digits(I));
         Difference := (L_Digit - R_Digit) - Borrow;
         if (Difference and 16#FF00#) /= 0 then
            Borrow := 1;
         else
            Borrow := 0;
         end if;
         Result.Long_Digits(I) := Octet(Difference and 16#00FF#);
      end loop;
      return Result;
   end ModSubtract;


   -- This is "Algorithm M" from Knuth's "The Art of Computer Programming, Volume 2: Semi-
   -- numerical Algorithms" (third edition, published by Addison-Wesley, copyright 1998, pages
   -- 268-270).
   --
   function ModMultiply(L, R : Very_Long) return Very_Long is
      Result : Very_Long(L.Length);
      L_Digit : Double_Octet;
      R_Digit : Double_Octet;
      T_Digit : Double_Octet;
      Temp    : Double_Octet;
      Carry   : Double_Octet;
   begin
      -- Prepare Result's digit array.
      Result.Long_Digits := (others => 16#00#);

      -- Do the multiplication.
      for J in R.Long_Digits'Range loop
         Carry := 0;
         for I in L.Long_Digits'Range loop
            L_Digit := Double_Octet(L.Long_Digits(I));
            R_Digit := Double_Octet(R.Long_Digits(J));
            if I + J - 1 in Result.Long_Digits'Range then
               T_Digit := Double_Octet(Result.Long_Digits(I + J - 1));
               Temp    := (L_Digit * R_Digit) + T_Digit + Carry;
               Result.Long_Digits(I + J - 1) := Octet(Temp and 16#00FF#);
               Carry   := Shift_Right(Temp, Digit_Bits);
            end if;
         end loop;
      end loop;
      return Result;
   end ModMultiply;


   function "*"(L, R : Very_Long) return Very_Long is
      Result  : Very_Long(L.Length + R.Length);
      L_Digit : Double_Octet;
      R_Digit : Double_Octet;
      T_Digit : Double_Octet;
      Temp    : Double_Octet;
      Carry   : Double_Octet;
   begin
      -- Prepare Result's digit array.
      Result.Long_Digits := (others => 16#00#);

      -- Do the multiplication.
      for J in R.Long_Digits'Range loop
         Carry := 0;
         for I in L.Long_Digits'Range loop
            L_Digit  := Double_Octet(L.Long_Digits(I));
            R_Digit  := Double_Octet(R.Long_Digits(J));
            T_Digit  := Double_Octet(Result.Long_Digits(I + J - 1));
            Temp     := (L_Digit * R_Digit) + T_Digit + Carry;
            Result.Long_Digits(I + J - 1) := Octet(Temp and 16#00FF#);
            Carry    := Shift_Right(Temp, Digit_Bits);
         end loop;
         Result.Long_Digits(L.Length + J) := Octet(Carry and 16#00FF#);
      end loop;
      return Result;
   end "*";


   procedure Divide
     (Dividend  : in  Very_Long;
      Divisor   : in  Very_Long;
      Quotient  : out Very_Long;
      Remainder : out Very_Long) is separate;

end Very_Longs;
