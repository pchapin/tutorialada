pragma SPARK_Mode(On);

with Interfaces.C;
use  Interfaces;

package C_Library is

   function Sum(X : in C.int; Y : in C.int) return C.int
     with
       Global => null,
       Import,
       Convention => C,
       External_Name => "sum";

   procedure Print_Int(Value : in C.int)
     with
       Import,
       Convention => C,
       External_Name => "print_int";

   type IntFormat is
      record
         Number    : C.int;
         Formatted : C.char_array(0 .. 127);
      end record
     with Convention => C;

   procedure Format_Integer(Value : in out IntFormat)
     with
       Global => null,
       Import,
       Convention => C,
       External_Name => "format_integer";


end C_Library;
