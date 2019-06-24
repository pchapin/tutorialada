---------------------------------------------------------------------------
-- FILE   : hello.adb
-- SUBJECT: The classic "Hello, World" program in Ada
--
-- To compile do 'gnatmake hello.adb' then run the resulting executable file.
---------------------------------------------------------------------------

with Ada.Text_IO;

procedure Hello is
begin
  Ada.Text_IO.Put_Line("Hello, Ada!");
end Hello;
