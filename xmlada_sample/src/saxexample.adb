
with Sax.Attributes; use Sax.Attributes;
with Ada.Text_IO;    use Ada.Text_IO;

package body SaxExample is

   overriding
   procedure Start_Element
     (Handler       : in out Reader;
      Namespace_URI : in Unicode.CES.Byte_Sequence := "";
      Local_Name    : in Unicode.CES.Byte_Sequence := "";
      Qname         : in Unicode.CES.Byte_Sequence := "";
      Atts          : in Sax.Attributes.Attributes'Class) is
   begin
      Handler.Current_Pref  := Null_Unbounded_String;
      Handler.Current_Value := Null_Unbounded_String;

      if Local_Name = "pref" then
         Handler.Current_Pref := To_Unbounded_String(Get_Value(Atts, "name"));
      end if;
   end Start_Element;

   overriding
   procedure Characters
     (Handler : in out Reader;
      Ch      : in Unicode.CES.Byte_Sequence) is
   begin
      if Handler.Current_Pref /= Null_Unbounded_String then
         Handler.Current_Value := Handler.Current_Value & Ch;
      end if;
   end Characters;

   overriding
   procedure End_Element
     (Handler       : in out Reader;
      Namespace_URI : in Unicode.CES.Byte_Sequence := "";
      Local_Name    : in Unicode.CES.Byte_Sequence := "";
      Qname         : in Unicode.CES.Byte_Sequence := "") is
   begin
      if Local_Name = "pref" then
         Put_Line("Value for """ & To_String(Handler.Current_Pref) &
                  """ is "       & To_String(Handler.Current_Value));
      end if;

   end End_Element;

end SaxExample;
