---------------------------------------------------------------------------
-- FILE    : datebook.adb
-- SUBJECT : Body of a package providing a simple datebook of events.
--
---------------------------------------------------------------------------

package body Datebook is

   use type Dates.Datetime;

   Maximum_Description_Length : constant := 128;
   subtype Description_Index_Type is Positive range 1 .. Maximum_Description_Length;
   subtype Description_Count_Type is Natural  range 0 .. Maximum_Description_Length;
   subtype Description_Type is String(Description_Index_Type);

   -- Each Event_Record handles exactly one datebook entry.
   type Event_Record is
      record
         Description_Text : Description_Type := (others => ' ');
         Description_Size : Description_Count_Type := 0;
         Date             : Dates.Datetime;
         Is_Used          : Boolean := False;
      end record;

   subtype Event_Index_Type is Positive range 1 .. Maximum_Number_Of_Events;
   type Event_Array_Type is array(Event_Index_Type) of Event_Record;

   -- The events are stored here in no particular order.
   Event_Array : Event_Array_Type;


   procedure Add_Event
     (Description : in     String;
      Date        : in     Dates.Datetime;
      Status      :    out Status_Type)
   is
      Found : Boolean;  -- Is there an available slot?
      Available : Event_Index_Type := Event_Index_Type'First;  -- Location of available slot.
   begin
      -- If the given description won't fit there is no point continuing.
      if Description'Length > Maximum_Description_Length then
         Status := Description_Too_Long;
      else
         -- Search for a free slot in the event array.
         Found := False;
         for Index in Event_Index_Type loop
            if not Event_Array(Index).Is_Used then
               Available := Index;
               Found := True;
               exit; -- We found a available slot in the array
            end if;
         end loop;

         if not Found then
            -- If there is no free slot return an error.
            Status := Insufficient_Space;
         else
            -- Otherwise fill in the free slot with the incoming information.
            -- Need to pad the description with blanks.
            Event_Array(Available).Description_Text := (others => ' ');
            Event_Array(Available).Description_Text(1 .. Description'Length) := Description;
            Event_Array(Available).Description_Size := Description'Length;
            Event_Array(Available).Date    := Date;
            Event_Array(Available).Is_Used := True;
            Status := Success;
         end if;
      end if;
   end Add_Event;


   procedure Purge_Events_Before(Date : in Dates.Datetime) is
   begin
      for Index in Event_Index_Type loop
         if Event_Array(Index).Date < Date then
            Event_Array(Index).Is_Used := False;
         end if;
      end loop;
   end Purge_Events_Before;


   procedure Get_Earliest_Event_Date(Date : out Dates.Datetime; Status : out Status_Type) is
      Default_Datetime : Dates.Datetime;
   begin
      Status := No_Event;
      Date := Default_Datetime;  -- A value to use in case there is No_Event.

      for Index in Event_Index_Type loop
         -- We found a real event...
         if Event_Array(Index).Is_Used then

            -- If this is the first event found, save it unconditionally.
            if Status = No_Event then
               Date   := Event_Array(Index).Date;
               Status := Success;
            else
               -- Otherwise save this event only if it is earlier than our best so far.
               if Event_Array(Index).Date < Date then
                  Date := Event_Array(Index).Date;
               end if;
            end if;
         end if;
      end loop;
   end Get_Earliest_Event_Date;


   procedure Get_Event
     (Date        : in     Dates.Datetime;
      Description :    out String;
      Status      :    out Status_Type)
   is
      Index : Event_Index_Type;
   begin
      Status := No_Event;
      Description := (others => ' ');

      Index := Event_Index_Type'First;
      loop
         -- Is this item in the event array the one we are looking for?
         if Event_Array(Index).Date = Date then
            if Description'Length < Event_Array(Index).Description_Size then
               Status := Description_Too_Long;
            else
               for J in 1 .. Event_Array(Index).Description_Size loop
                  Description(Description'First + (J - 1)) :=
                    Event_Array(Index).Description_Text(J);
               end loop;
               Status := Success;
            end if;
            exit;
         end if;

         -- If not, then advance to the next item.
         exit when Index = Event_Index_Type'Last;
         Index := Index + 1;
      end loop;
   end Get_Event;


   procedure Get_Next_Event_Date
     (Current_Date : in     Dates.Datetime;
      Next_Date    :    out Dates.Datetime;
      Status       :    out Status_Type)
   is

      -- Returns True if D1 is after D2; False otherwise.
      function ">"(D1 : Dates.Datetime; D2 : Dates.Datetime) return Boolean
        with Global => null
      is
      begin
         return not (D1 < D2) and D1 /= D2;
      end ">";

      Default_Datetime : Dates.Datetime;

   begin
      Status := No_Event;
      Next_Date := Default_Datetime;  -- A value to use in case there is No_Event.

      for Index in Event_Index_Type loop
         if Event_Array(Index).Is_Used and then Event_Array(Index).Date > Current_Date then
            Next_Date := Event_Array(Index).Date;
            Status := Success;
            exit;
         end if;
      end loop;
   end Get_Next_Event_Date;


   function Event_Count return Event_Count_Type is
      Count : Event_Count_Type := 0;
   begin
      for Index in Event_Index_Type loop
         if Event_Array(Index).Is_Used then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Event_Count;

end Datebook;
