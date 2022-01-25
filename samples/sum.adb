-- This program computes the sum of the first N integers using a loop. This result can also
-- be calculated with (N*(N+1))/2, but this program uses a loop for illustrative purposes.
-- Notice that Constraint_Error is raised if a large value of N is used. This is because the
-- range on Integer is overflowed during the computation.
--
with Ada.Text_IO;
with Ada.Integer_Text_IO;

use Ada.Text_IO;
use Ada.Integer_Text_IO;

procedure Sum is
   N   : Integer;
   Sum : Integer := 0;
begin
   Put("Enter an integer: ");
   Get(N);

   for I in 1 .. N loop
      Sum := Sum + I;
   end loop;

   Put("The sum of all integers up to "); Put(N, 0); Put(" is "); Put(Sum, 0); New_Line;
end Sum;
