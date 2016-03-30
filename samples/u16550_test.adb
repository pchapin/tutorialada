---------------------------------------------------------------------------
-- FILE          : u16550_test.adb
-- LAST REVISION : 2008-06-22
-- SUBJECT       : Demoonstration program for the 16550 UART handling package.
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
with U16550; use U16550;

procedure U16550_Test is
begin
   Initialize_Port( COM1, B9600,
                    (Word_Length   => Bits_8,
                     N_Stop_Bits   => One_Bit,
                     Parity_Enable => True,
                     Parity        => Even,
                     Stick_Parity  => False,
                     Set_Break     => False,
                     DLAB_Enable   => False) );
end U16550_Test;

