---------------------------------------------------------------------------
-- FILE    : producer.adb
-- SUBJECT : A task to produce work items.
-- AUTHOR  : (C) Copyright 2012 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

package body Producer is
   Primary_Timer : Timer;

   task body Timer is
      Release_Time  : Ada.Real_Time.Time := Epoch.Producer_Start;
      Item          : Natural := 0;
   begin
      loop
         delay until Release_Time;
         Item := Item + 1;           -- Produce an item.
         Work_Q.Mailbox.Add(Item);
         Release_Time := Release_Time + Epoch.Consumer_Period;
      end loop;
   end Timer;

end Producer;
