---------------------------------------------------------------------------
-- FILE   : random_test.adb
-- SUBJECT: Simple demonstration/test program for package Random.
--
---------------------------------------------------------------------------
with Ada.Text_IO;
with Random;

procedure Random_Test is

   package Random_IO is new Ada.Text_IO.Integer_IO(Random.Number_Type);

   Gen : Random.Generator := Random.Make(Seed => 1);
   Num : Random.Number_Type;

begin
   for I in 1 .. 10 loop
      Random.Next(Gen, Num);
      Random_IO.Put(Num);
      Ada.Text_IO.New_Line;
   end loop;

   Ada.Text_IO.New_Line;

   for I in 1 .. 10 loop
      Random.Next(Gen, Low => 1, High => 6, Number => Num);
      Random_IO.Put(Num);
      Ada.Text_IO.New_Line;
   end loop;
end Random_Test;
