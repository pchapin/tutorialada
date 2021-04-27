---------------------------------------------------------------------------
-- FILE          : channel_test.adb
-- LAST REVISION : 2021-04-27
-- SUBJECT       : Program to exercise the simulated noisy channel.
-- PROGRAMMER    : (C) Copyright 2021 by Peter C. Chapin
--
-- This program could be better. It should collect statistical data to measure the bit error rate of the
-- simulated channel and compare that to the configured rate. It should probably also attempt to verify that the
-- channel's errors are random.
---------------------------------------------------------------------------

with Channel; use Channel;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Channel_Test is

   Error_Rate : Float;
   Loop_Count : Integer;

   package Int_IO is new Integer_IO(Integer); use Int_IO;
   package Flt_IO is new Float_IO(Float);     use Flt_IO;
   package Mod_IO is new Modular_IO(Octet);   use Mod_IO;
   package Random_Octet is new Ada.Numerics.Discrete_Random(Octet);

   Gen : Random_Octet.Generator;

   -- Generic procedure to read a number from the user.
   generic
      type Data_Type is private;
      with procedure Get(Data : out Data_Type; Width : in Field := 0);
      with function "<"(L : in Data_Type; R : in Data_Type) return Boolean is <>;
      with function ">"(L : in Data_Type; R : in Data_Type) return Boolean is <>;
   procedure Read_Value
     (Data          : out Data_Type;
      Lower_Bound   : in  Data_Type;
      Upper_Bound   : in  Data_Type;
      Prompt        : in  String;
      Error_Message : in  String);

  -- Implementation of the generic procedure.
   procedure Read_Value
     (Data          : out Data_Type;
      Lower_Bound   : in  Data_Type;
      Upper_Bound   : in  Data_Type;
      Prompt        : in  String;
      Error_Message : in  String)
   is
      Retry       : Boolean;
      Trial_Value : Data_Type;
   begin
      loop
         Retry := FALSE;
         Put(Prompt);
         Get(Trial_Value);
         Skip_Line;
         if Trial_Value > Upper_Bound or Trial_Value < Lower_Bound then
            Put_Line(Error_Message);
            Retry := TRUE;
         end if;
         exit when Retry = FALSE;
      end loop;
      Data := Trial_Value;
   end Read_Value;

   procedure Get_Float_Value is
     new Read_Value(Data_Type => Float, Get => Flt_IO.Get);

   procedure Get_Integer_Value is
     new Read_Value(Data_Type => Integer, Get => Int_IO.Get);

   procedure Print_Header is
   begin
      Put_Line("Channel_Test  (Version 1.0a, 2003-12-28)");
      New_Line(2);
   end Print_Header;

begin -- Channel_Test

   Print_Header;
   Random_Octet.Reset(Gen);
   Channel.Initialize;

   Get_Float_Value
     (Data          => Error_Rate,
      Lower_Bound   => 0.0,
      Upper_Bound   => 0.5,
      Prompt        => "Enter a value for the bit error rate: ",
      Error_Message => "Sorry, I can only accept values between 0.0 and 0.5.");

   Get_Integer_Value
     (Data          => Loop_Count,
      Lower_Bound   => 0,
      Upper_Bound   => Integer'LAST,
      Prompt        => "Enter a value for the loop count: ",
      Error_Message => "Sorry, I can only accept values greater than zero.");

   Channel.Error_Rate(New_Rate => Error_Rate);

   MAIN_LOOP:
   declare
      Sent          : Octet;
      Got           : Octet;
      Number_Of_Bad : Integer := 0;

   begin
      for I in 1 .. Loop_Count loop

         -- Come up with random number and send it down the channel.
         Sent := Random_Octet.Random(Gen);
         Got  := Sent;
         Transceive(Got);

         -- Print results.
         Put("Octet number:"); Put(I);
         Put("    Sent:");     Put(Sent);
         Put("    Got:");      Put(Got);

         -- Let user know, if this byte was screwed up.
         if Sent /= Got then
            Put(" *****");
            Number_Of_Bad := Number_Of_Bad + 1;
         end if;
         New_Line;
      end loop;

      -- Print summary.
      New_Line(2);
      Put("Found "); Put(Number_Of_Bad); Put(" bad bytes."); New_Line;
   end MAIN_LOOP;

end Channel_Test;
