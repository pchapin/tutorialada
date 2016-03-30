---------------------------------------------------------------------------
-- FILE    : epoch.ads
-- SUBJECT : Package containing task starting information..
-- AUTHOR  : (C) Copyright 2012 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Ada.Real_Time;
use type Ada.Real_Time.Time;
--# inherit Ada.Real_Time;

package Epoch is
   Start_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

   Consumer_Start  : constant Ada.Real_Time.Time      := Start_Time + Ada.Real_Time.Milliseconds(100);
   Consumer_Period : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(200);

   Producer_Start  : constant Ada.Real_Time.Time      := Start_Time + Ada.Real_Time.Milliseconds(50);
   Producer_Period : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(200);
end Epoch;
