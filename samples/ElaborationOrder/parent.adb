with Parent.Child;

pragma Elaborate(Parent.Child);

package body Parent is

   procedure Dummy is
   begin
      null;
   end;

begin
   Parent.Child.Print_Stuff;
end Parent;
