---------------------------------------------------------------------------
-- FILE   : rationals_test.adb
-- SUBJECT: Program to exercise package Rational.
--
---------------------------------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Rationals;

procedure Rationals_Test is
   package Integer_Rationals is new Rationals(Integer);
   use Integer_Rationals;

   procedure Put(R : Rational) is
   begin
      Put(Get_Numerator(R), Width => 0);
      Put("/");
      Put(Get_Denominator(R), Width => 0);
   end Put;

   X, Y, Z : Rational;
begin
   X := Make(1, 3);
   Y := Make(1, 2);

   Z := X + Y;
   Put("1/3 + 1/2 = "); Put(Z); New_Line;

   Z := X - Y;
   Put("1/3 - 1/2 = "); Put(Z); New_Line;

   Z := X * Y;
   Put("1/3 * 1/2 = "); Put(Z); New_Line;

   Z := X / Y;
   Put("1/3 / 1/2 = "); Put(Z); New_Line;

   Z := Make(6, 12);
   Put("6/12 reduced is "); Put(Z); New_Line;
end Rationals_Test;
