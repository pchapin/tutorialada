with Ada.Text_IO; use Ada.Text_IO;

package body Base is

   procedure P1( X : Base_Type ) is
   begin
      Put_Line("Base.P1");
   end P1;

   procedure P2( X : out Base_Type ) is
   begin
      Put_Line("Base.P2");
      X.B := 0;
   end P2;

   procedure P3( X: Base_Type; Y : Base_Type ) is
   begin
      Put_Line("Base.P3");
   end;

   function F return Base_Type is
   begin
      Put_Line("Base.F");
      return ( B => 0 );
   end F;

end Base;
