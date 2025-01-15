with Sax.Readers;
with Unicode.CES;
with Sax.Attributes;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package SaxExample is
   type String_Access is access String;

   type Reader is new Sax.Readers.Reader with
      record
         Current_Pref  : Unbounded_String;
         Current_Value : Unbounded_String;
      end record;

   overriding
   procedure Start_Element
     (Handler       : in out Reader;
      Namespace_URI : in Unicode.CES.Byte_Sequence := "";
      Local_Name    : in Unicode.CES.Byte_Sequence := "";
      Qname         : in Unicode.CES.Byte_Sequence := "";
      Atts          : in Sax.Attributes.Attributes'Class);

   overriding
   procedure End_Element
     (Handler       : in out Reader;
      Namespace_URI : in Unicode.CES.Byte_Sequence := "";
      Local_Name    : in Unicode.CES.Byte_Sequence := "";
      Qname         : in Unicode.CES.Byte_Sequence := "");

   overriding
   procedure Characters
     (Handler : in out Reader;
      Ch      : in Unicode.CES.Byte_Sequence);

end SaxExample;
