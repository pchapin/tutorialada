---------------------------------------------------------------------------
-- FILE      : hello.adb
-- SUBJECT   : Demonstration of Win32Ada.
-- PROGRAMMER: (C) Copyright 2015 by Peter C. Chapin
--
---------------------------------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with System;
with Win32.Windef;
with Win32.Winuser; use Win32.Winuser;

-- function MessageBox(hWnd      : Win32.Windef.HWND;
--                     lpText    : Win32.LPCSTR;
--                     lpCaption : Win32.LPCSTR;
--                     uType     : Win32.UINT) return Win32.INT renames MessageBoxA;

procedure Hello is
   Result  : Win32.INT;
   Message : char_array := To_C("Hello, World!");
   Caption : char_array := To_C("Win32Ada");
begin
   Result := MessageBox
      (hWnd      => System.Null_Address,
       lpText    => Message(0)'Unchecked_Access,
       lpCaption => Caption(0)'Unchecked_Access,
       uType     => MB_OKCANCEL);

   case Result is
      when IDCANCEL =>
         Put_Line("You pressed 'Cancel'");

      when IDOK =>
         Put_Line("You pressed 'OK'");

      when others =>
         Put_Line("You pressed something, but I don't know what.");
   end case;

end Hello;

