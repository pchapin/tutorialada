with Ada.Text_IO;

package body Parent.Child is

   procedure Print_Stuff is
   begin
      Ada.Text_IO.Put_Line("Printing stuff in package Parent.Child");
   end Print_Stuff;

end Parent.Child;
