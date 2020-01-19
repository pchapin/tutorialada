with Base;

package Derived is

   type Derived_Type is new Base.Base_Type with
     record
        D : Integer;
     end record;

   overriding procedure P1(X : Derived_Type);
   overriding procedure P2(X : out Derived_Type);
   overriding procedure P3(X : Derived_Type; Y : Derived_Type);
   overriding function F return Derived_Type;


end Derived;
