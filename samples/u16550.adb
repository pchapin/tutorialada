---------------------------------------------------------------------------
-- FILE          : u16550.adb
-- SUBJECT       : Implementation of a 16550 UART handling package.
---------------------------------------------------------------------------
with Interfaces; use Interfaces;

package body U16550 is

   Divisors : array(Baud_Type) of Unsigned_16 := (384, 192, 96, 48, 24, 12, 6, 3);

   -- Configure the specified port with parameters as provided.
   procedure Initialize_Port
      (Port : Port_Type;
       Baud : Baud_Type;
       Line_Parameters : Line_Control_Type) is
   begin
      null;
   end Initialize_Port;

   -- Implementation of other package subprograms.

end U16550;
