---------------------------------------------------------------------------
-- FILE    : work_q.adb
-- SUBJECT : Package containing a place where work waits until the server can handle.
-- AUTHOR  : (C) Copyright 2012 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

package body Work_Q is

   protected body Mailbox_Type is

      entry Add(Item : in Natural) when Space_Available
      --# global in out Buffer, Next_In, Current_Count; out Space_Available;
      --# derives Buffer          from Buffer, Next_In, Item &
      --#         Next_In         from Next_In               &
      --#         Current_Count   from Current_Count         &
      --#         Space_Available from Current_Count;
      is
      begin
         Buffer(Next_In) := Item;
         Next_In         := Next_In + 1;
         Current_Count   := Current_Count + 1;

         if Current_Count = Maximum_Buffer_Size then
            Space_Available := False;
         else
            Space_Available := True;
         end if;
      end Add;


      procedure Remove(Item : out Natural)
      --# global in Buffer; in out Next_Out, Current_Count; out Space_Available;
      --# derives Next_Out        from Current_Count, Next_Out         &
      --#         Current_Count   from Current_Count                   &
      --#         Space_Available from                                 &
      --#         Item            from Current_Count, Buffer, Next_Out;
      is
      begin
         if Current_Count = 0 then
            Item := 0;
         else
            Item          := Buffer(Next_Out);
            Next_Out      := Next_Out + 1;
            Current_Count := Current_Count - 1;
         end if;
         Space_Available := True;
      end Remove;


      function Count return Buffer_Count_Type
      --# global in Current_Count;
      is
      begin
         return Current_Count;
      end Count;

   end Mailbox_Type;

end Work_Q;
