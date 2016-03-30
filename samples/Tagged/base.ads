
package Base is

   type Base_Type is tagged
      record
         B : Integer;
      end record;

   procedure P1( X : Base_Type );
   procedure P2( X : out Base_Type );
   procedure P3( X : Base_Type; Y : Base_Type );
   function F return Base_Type;


end Base;
