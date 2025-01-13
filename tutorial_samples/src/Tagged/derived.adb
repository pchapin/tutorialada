with Ada.Text_IO; use Ada.Text_IO;

package body Derived is

   procedure P1( X : Derived_Type ) is
   begin
      Put_Line("Derived.P1");
   end P1;

   procedure P2( X : out Derived_Type ) is
   begin
      Put_Line("Derived.P2");
      X.B := 0;
      X.D := 1;
   end P2;

   procedure P3( X: Derived_Type; Y : Derived_Type ) is
   begin
      Put_Line("Derived.P3");
   end P3;

   function F return Derived_Type is
   begin
      Put_Line("Derived.F");
      return ( B => 0, D => 1 );
   end F;

end Derived;
