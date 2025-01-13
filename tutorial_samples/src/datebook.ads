---------------------------------------------------------------------------
-- FILE    : datebook.ads
-- SUBJECT : Package providing a simple datebook of events.
--
---------------------------------------------------------------------------

with Dates;

package Datebook is

   Maximum_Number_Of_Events : constant := 64;
   subtype Event_Count_Type is Natural range 0 .. Maximum_Number_Of_Events;

   type Status_Type is (Success, Description_Too_Long, Insufficient_Space, No_Event);

   -- Adds an event to the datebook.
   --   Fails with Description_Too_Long if the description string is too large to store.
   --   Fails with Insufficient_Space if the datebook is full.
   procedure Add_Event
     (Description : in     String;
      Date        : in     Dates.Datetime;
      Status      :    out Status_Type);

   -- Removes all events before the given date. This procedure can't fail.
   procedure Purge_Events_Before(Date : in Dates.Datetime);

   -- Returns the date associated with the earliest event.
   --   Fails with No_Event if the datebook is empty.
   procedure Get_Earliest_Event_Date(Date : out Dates.Datetime; Status : out Status_Type);

   -- Returns the description on the event at the given date.
   --   Fails with No_Event if there are no event on the given date.
   --   Fails with Description_Too_Long if the Description string isn't large enough to receive
   --   the description text.
   procedure Get_Event
     (Date        : in     Dates.Datetime;
      Description :    out String;
      Status      :    out Status_Type);

   -- Returns the first date associated with an event after the Current_Date.
   --   Fails with No_Event if there is no later event.
   procedure Get_Next_Event_Date
     (Current_Date : in     Dates.Datetime;
      Next_Date    :    out Dates.Datetime;
      Status       :    out Status_Type);

   -- Returns the number of events currently in the datebook.
   function Event_Count return Event_Count_Type;

end Datebook;
