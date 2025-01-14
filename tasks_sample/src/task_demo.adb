with Ada.Text_IO;
use  Ada.Text_IO;

package body Task_Demo is

   procedure Check_Prime (N : Natural; Result : out Boolean) is

      --  This function returns True if N is prime; False otherwise.
      --  Note that it assumes N >= 2.
      function Is_Prime (N : Integer) return Boolean is
      begin
         for I in 2 .. (N - 1) loop
            if N rem I = 0 then
               return False;
            end if;
         end loop;
         return True;
      end Is_Prime;

      task Print_Dots is
         entry Stop;
      end Print_Dots;

      task body Print_Dots is
         Done : Boolean := False;
      begin
         while not Done loop
            select
               accept Stop do
                  Done := True;
               end Stop;
            or
               delay 1.0;
               Put (".");
            end select;
         end loop;
      end Print_Dots;

   begin
      Result := Is_Prime (N);
      Print_Dots.Stop;
   end Check_Prime;

end Task_Demo;
