with Ada.Text_IO;

-- This procedure is intended to demonstrate scalar subtypes.
procedure Display_Graphic_Demo is

   -- A package to manage a hypothetical 128x256 pixel graphic display in an embedded system.
   -- In real life this package would more likely be at library level.
   package Graphic_Display is

      subtype X_Coordinate_Type is Integer range 0 .. 255;
      subtype Y_Coordinate_Type is Integer range 0 .. 127;

      type Pixel_Color_Type is (Red, Green, Blue);

      type Pixel_Intensity_Type is range 0 .. 255;  -- 0 is black, 255 is maxiumum brightness.

      procedure Set_Pixel_Color_Component
        (X : in X_Coordinate_Type;
         Y : in Y_Coordinate_Type;
         Color      : Pixel_Color_Type;
         Brightness : Pixel_Intensity_Type);

   end Graphic_Display;


   package body Graphic_Display is

      procedure Set_Pixel_Color_Componet
        (X : in X_Coordinate_Type;
         Y : in Y_Coordinate_Type;
         Color      : Pixel_Color_Type;
         Brightness : Pixel_Intensity_Type)
      is
      begin
         -- In real life, this would communicate with the display.
         Ada.Text_IO.Put_Line
           ("X: "   & X_Coordinate_Type'Image(X) &
            ", Y: " & Y_Coordinate_Type'Image(Y) &
            ", Color: "      & Pixel_Color_Type'Image(Color) &
            ", Brightness: " & Pixel_Intensity_Type'Image(Brightness) );
      end Set_Pixel_Color_Componet;

   end Graphic_Display;

begin
   -- Clear the screen.
   for Color in Graphic_Display.Pixel_Color_Type loop
      for X_Coordinate in Graphic_Display.X_Coordinate_Type loop
         for Y_Coordinate in Graphic_Display.Y_Coordinate_Type loop
            Graphic_Display.Set_Pixel_Color_Component(X_Coordinate, Y_Coordinate, Color, Brightness => 0);
         end loop;
      end loop;
   end loop;
end Display_Graphic_Demo;
