with Base;

package Derived is

   type Derived_Type is new Base.Base_Type with
     record
        D : Integer;
     end record;

   procedure P1( X : Derived_Type );
   procedure P2( X : out Derived_Type );
   procedure P3( X : Derived_Type; Y : Derived_Type );
   function F return Derived_Type;


end Derived;
