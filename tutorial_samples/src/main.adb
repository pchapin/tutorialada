-- This main program is used in VTC's CIS-2730, Lab #2.

-- Some packages that we will need.
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Number_Theory;

-- Make the contents of the standard library packages "directly visible."
use Ada.Text_IO;
use Ada.Integer_Text_IO;

procedure Main is
   -- We need to print values of type Number_Theory.Floating_Type, so generate a package for that.
   package Floating_IO is new Ada.Text_IO.Float_IO(Number_Theory.Floating_Type);
   use Floating_IO;

   N : Number_Theory.Prime_Argument_Type;
   Exact_Prime_Count : Natural;
   Approximate_Prime_Count : Number_Theory.Floating_Type;

begin
   Put("Enter a number: ");
   Get(N);
   Exact_Prime_Count := Number_Theory.Prime_Counting(N);
   Put(" π(N) = "); Put(Exact_Prime_Count, 0); New_Line;
   Approximate_Prime_Count := Number_Theory.Logarithmic_Integral(N);
   Put("li(N) = "); Put(Approximate_Prime_Count, 0); New_Line;
end Main;
