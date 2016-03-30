---------------------------------------------------------------------------
-- FILE    : consumer.ads
-- SUBJECT : A task to consume work items.
-- AUTHOR  : (C) Copyright 2012 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with Ada.Real_Time;
use type Ada.Real_Time.Time;
with System;
with Epoch;
with Work_Q;

--# inherit Ada.Real_Time, System, Epoch, Work_Q;
package Consumer
--# own task Primary_Timer : Timer;
is
private

   task type Timer
   --# global in out Work_Q.Mailbox;
   --# derives Work_Q.Mailbox from Work_Q.Mailbox;
   is
      pragma Priority(System.Default_Priority);
   end Timer;

end Consumer;
