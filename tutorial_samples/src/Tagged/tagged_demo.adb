with Ada.Text_IO; use Ada.Text_IO;
with Base;
with Derived;

procedure Tagged_Demo is

   procedure Test(X : Base.Base_Type'Class) is
      Y : Base.Base_Type'Class := X;
   begin
      Base.P1(X);
      Base.P2(Y);
      Y := Base.F;
   end Test;

   procedure Test2 (X, Y : Base.Base_Type'Class) is
   begin
      Base.P3(X, Y);
   end Test2;

   X : constant Base.Base_Type := (B => 10);
   Y : constant Derived.Derived_Type := (B => 10, D => 20);
begin

   Put_Line("Testing regular types.");
   Base.P1(X);
   -- Base.P1(Y);  -- Compile error due to type mismatch.
   -- Derived.P1(X);  -- Compile error due to type mismatch.
   Derived.P1(Y);

   New_Line;
   Put_Line("Testing class-wide types.");
   Test(X);
   Test(Y);
   Test2(X, X);
   -- Test2(X, Y);  -- Raises Constraint_Error at run time.
   -- Test2(Y, X);  -- Raises Constraint_Error at run time.
   Test2(Y, Y);
end Tagged_Demo;
