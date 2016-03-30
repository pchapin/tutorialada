---------------------------------------------------------------------------
-- FILE    : consumer.adb
-- SUBJECT : A task to consume work items.
-- AUTHOR  : (C) Copyright 2012 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

package body Consumer is
   Primary_Timer : Timer;

   task body Timer is
      Release_Time  : Ada.Real_Time.Time := Epoch.Consumer_Start;
      Item          : Natural;
      Mailbox_Count : Work_Q.Buffer_Count_Type;
   begin
      loop
         delay until Release_Time;
         Mailbox_Count := Work_Q.Mailbox.Count;
         if Mailbox_Count > 0 then
            Work_Q.Mailbox.Remove(Item);
         end if;
         Release_Time := Release_Time + Epoch.Consumer_Period;
      end loop;
   end Timer;

end Consumer;
