---------------------------------------------------------------------------
-- FILE   : u16550.ads
-- SUBJECT: Specification of a 16550 UART handling package.
--
-- This package is intended to illustrate some of Ada's support for low level hardware control.
---------------------------------------------------------------------------
with System;

package U16550 is

   type Port_Type is (COM1, COM2);
   type Baud_Type is (B300, B600, B1200, B2400, B4800, B9600, B19200, B38400);

   type Word_Length_Type is  (Bits_5, Bits_6, Bits_7, Bits_8);
   for  Word_Length_Type use (Bits_5 => 0, Bits_6 => 1, Bits_7 => 2, Bits_8 => 3);

   type Stop_Bits_Type is  (One_Bit, Two_Bits);
   for  Stop_Bits_Type use (One_Bit => 0, Two_Bits => 1);

   type Parity_Type is  (Odd, Even);
   for  Parity_Type use (Odd => 0, Even => 1);

   type Line_Control_Type is
      record
         Word_Length   : Word_Length_Type;
         N_Stop_Bits   : Stop_Bits_Type;
         Parity_Enable : Boolean;
         Parity        : Parity_Type;
         Stick_Parity  : Boolean;
         Set_Break     : Boolean;
         DLAB_Enable   : Boolean;
      end record;

   for Line_Control_Type'Bit_Order use System.Low_Order_First;
   for Line_Control_Type use
      record
         Word_Length   at 0 range 0..1;  -- Specify bit positions.
         N_Stop_Bits   at 0 range 2..2;
         Parity_Enable at 0 range 3..3;
         Parity        at 0 range 4..4;
         Stick_Parity  at 0 range 5..5;
         Set_Break     at 0 range 6..6;
         DLAB_Enable   at 0 range 7..7;
      end record;

   procedure Initialize_Port
      (Port : Port_Type; Baud : Baud_Type; Line_Parameters : Line_Control_Type);

   -- Other subprograms to read/write the port, check its status, etc.

end U16550;
