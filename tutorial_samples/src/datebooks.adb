---------------------------------------------------------------------------
-- FILE    : datebooks.adb
-- SUBJECT : Body of a package providing a simple datebook type.
--
---------------------------------------------------------------------------

package body Datebooks is
   use type Dates.Datetime;

   procedure Add_Event
     (Book        : in out Datebook;
      Description : in     String;
      Date        : in     Dates.Datetime;
      Status      :    out Status_Type) is

      type Find_Result_Record is
         record
            Fresh_Index : Event_Index_Type;
            Found_Slot  : Boolean;
         end record;

      Find_Result : Find_Result_Record;
   begin
      Find_Result := (Fresh_Index => 1, Found_Slot => False);

      -- If the given description won't fit there is no point continuing.
      if Description'Length > Maximum_Description_Length then
         Status := Description_Too_Long;
      else
         -- Search for a free slot in the event array.
         for I in Event_Index_Type loop
            if not Book(I).Is_Used then
               Find_Result := (Fresh_Index => I, Found_Slot => True);
               exit;
            end if;
         end loop;

         -- If there is no free slot return an error.
         if not Find_Result.Found_Slot then
            Status := Insufficient_Space;
         else
            -- Otherwise fill in the free slot with the incoming information.
            Book(Find_Result.Fresh_Index).Description_Text := (others => ' ');
            Book(Find_Result.Fresh_Index).Description_Text(1 .. Description'Length) := Description;
            Book(Find_Result.Fresh_Index).Description_Size := Description'Length;
            Book(Find_Result.Fresh_Index).Date             := Date;
            Book(Find_Result.Fresh_Index).Is_Used          := True;
            Status := Success;
         end if;
      end if;
   end Add_Event;


   procedure Purge_Events_Before(Book : in out Datebook; Date : in Dates.Datetime) is
   begin
      for I in Event_Index_Type loop
         if Book(I).Date < Date then
            Book(I).Is_Used := False;
         end if;
      end loop;
   end Purge_Events_Before;


   procedure Get_Earliest_Event_Date
     (Book : in Datebook; Date : out Dates.Datetime; Status : out Status_Type) is

      Default_Datetime : Dates.Datetime;
   begin
      Status := No_Event;
      Date := Default_Datetime;  -- A value to use in case there is No_Event.

      for I in Event_Index_Type loop
         -- If we found a real event...
         if Book(I).Is_Used then

            -- If this is the first event found, save it unconditionally.
            if Status = No_Event then
               Date   := Book(I).Date;
               Status := Success;
            else
               -- Otherwise save this event only if it is earlier than our best so far.
               if Book(I).Date < Date then
                  Date := Book(I).Date;
               end if;
            end if;
         end if;
      end loop;
   end Get_Earliest_Event_Date;


   procedure Get_Event
     (Book        : in     Datebook;
      Date        : in     Dates.Datetime;
      Description :    out String;
      Status      :    out Status_Type) is

      I : Event_Index_Type;
   begin
      Status := No_Event;
      Description := (others => ' ');

      I := Event_Index_Type'First;
      loop
         -- Is this item in the event array the one we are looking for?
         if Book(I).Date = Date then
            if Description'Length < Book(I).Description_Size then
               Status := Description_Too_Long;
            else
               for J in Description_Count_Type range 1 .. Book(I).Description_Size loop
                  Description(Description'First + (J - 1)) := Book(I).Description_Text(J);
               end loop;
               Status := Success;
            end if;
            exit;
         end if;

         -- If not, then advance to the next item.
         exit when I = Event_Index_Type'Last;
         I := I + 1;
      end loop;
   end Get_Event;


   procedure Get_Next_Event_Date
     (Book         : in     Datebook;
      Current_Date : in     Dates.Datetime;
      Next_Date    :    out Dates.Datetime;
      Status       :    out Status_Type) is

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

      for I in Event_Index_Type loop
         if Book(I).Is_Used and then Book(I).Date > Current_Date then
            Next_Date := Book(I).Date;
            Status    := Success;
            exit;
         end if;
      end loop;
   end Get_Next_Event_Date;


   function Event_Count(Book : in Datebook) return Event_Count_Type is
      Count : Event_Count_Type := 0;
   begin
      for I in Event_Index_Type loop
         if Book(I).Is_Used then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Event_Count;

end Datebooks;
