---------------------------------------------------------------------------
-- FILE          : u16550.adb
-- LAST REVISION : 2008-06-22
-- SUBJECT       : Implementation of a 16550 UART handling package.
-- PROGRAMMER    : (C) Copyright 2008 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin
--      Computer Information Systems
--      Vermont Technical College
--      Randolph Center, VT 05061
--      Peter.Chapin@vtc.vsc.edu
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

