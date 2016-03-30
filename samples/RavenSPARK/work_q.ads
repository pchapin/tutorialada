---------------------------------------------------------------------------
-- FILE    : work_q.ads
-- SUBJECT : Package containing a place where work waits until the server can handle.
-- AUTHOR  : (C) Copyright 2012 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
with System;

--# inherit System;
package Work_Q
--# own protected Mailbox : Mailbox_Type (priority => System.Default_Priority, suspendable);
is
   Maximum_Buffer_Size : constant := 16;
   type Buffer_Index_Type is mod Maximum_Buffer_Size;
   subtype Buffer_Count_Type is Natural range 0 .. Maximum_Buffer_Size;
   type Buffer_Type is array(Buffer_Index_Type) of Natural;

   protected type MailBox_Type is
      pragma Priority(System.Default_Priority);

      entry Add(Item: in Natural);
      --# global in out Mailbox_Type;
      --# derives Mailbox_Type from Mailbox_Type, Item;

      procedure Remove(Item : out Natural);
      --# global in out Mailbox_Type;
      --# derives Mailbox_Type from Mailbox_Type &
      --#         Item         from Mailbox_Type;

      function Count return Buffer_Count_Type;
      --# global in Mailbox_Type;

   private
      Buffer          : Buffer_Type := Buffer_Type'(others => 0);
      Current_Count   : Buffer_Count_Type := 0;
      Next_In         : Buffer_Index_Type := 0;  -- Index of next slot to fill.
      Next_Out        : Buffer_Index_Type := 0;  -- Index of next item to remove.
      Space_Available : Boolean := True;
   end MailBox_Type;

   Mailbox : Mailbox_Type;

end Work_Q;
