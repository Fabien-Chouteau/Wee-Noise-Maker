-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2017 Fabien Chouteau                  --
--                                                                           --
--    Wee Noise Maker is free software: you can redistribute it and/or       --
--    modify it under the terms of the GNU General Public License as         --
--    published by the Free Software Foundation, either version 3 of the     --
--    License, or (at your option) any later version.                        --
--                                                                           --
--    Wee Noise Maker is distributed in the hope that it will be useful,     --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU       --
--    General Public License for more details.                               --
--                                                                           --
--    You should have received a copy of the GNU General Public License      --
--    along with We Noise Maker. If not, see <http://www.gnu.org/licenses/>. --
--                                                                           --
-------------------------------------------------------------------------------

with WNM.GUI.Bitmap_Fonts; use WNM.GUI.Bitmap_Fonts;

package body WNM.GUI.Menu.Drawing is

   Title_Y_Offset : constant := 10;
   Scroll_Bar_Y_Offset : constant := 19;

   Value_Text_Y : constant := Box_Bottom - 15;

   Arrow_Y_Offset : constant :=
     Box_Top + ((Box_Bottom - Box_Top + 1) - Bitmap_Fonts.Height) / 2;

   -------------------
   -- Draw_Menu_Box --
   -------------------

   procedure Draw_Menu_Box
     (Title       : String;
      Count       : Positive;
      Index       : Natural)
   is
      X : Integer;
   begin

      X := (Screen.Width - Title'Length * Bitmap_Fonts.Width) / 2;
      Print (X_Offset    => X,
             Y_Offset    => Title_Y_Offset,
             Str         => Title);

      declare
         X_Offset : constant Natural := (Screen.Width - Count * 3) / 2;
      begin
         for Item in 0 .. Count - 1 loop
            Screen.Set_Pixel ((X_Offset + Item * 3, Scroll_Bar_Y_Offset));
            Screen.Set_Pixel ((X_Offset + Item * 3 + 1, Scroll_Bar_Y_Offset));
         end loop;

         Screen.Set_Pixel ((X_Offset + Index * 3, Scroll_Bar_Y_Offset + 1));
         Screen.Set_Pixel ((X_Offset + Index * 3 + 1, Scroll_Bar_Y_Offset + 1));
      end;

      if Index < Count - 1 then
         X := Box_Right + 1;
         Print (X_Offset => X,
                Y_Offset => Arrow_Y_Offset,
                C        => '>');
      end if;

      --  Top line
      Screen.Draw_Line (Start => (Box_Left + 2, Box_Top),
                        Stop  => (Box_Right - 2, Box_Top));
      --  Bottom line
      Screen.Draw_Line (Start => (Box_Left + 2, Box_Bottom),
                        Stop  => (Box_Right - 2, Box_Bottom));

      --  Side left
      Screen.Draw_Line (Start => (Box_Left, Box_Top + 2),
                        Stop  => (Box_Left, Box_Bottom - 2));
      --  Side right
      Screen.Draw_Line (Start => (Box_Right, Box_Top + 2),
                        Stop  => (Box_Right, Box_Bottom - 2));

      Screen.Set_Pixel ((Box_Left + 1, Box_Top + 1));
      Screen.Set_Pixel ((Box_Left + 1, Box_Bottom - 1));
      Screen.Set_Pixel ((Box_Right - 1, Box_Top + 1));
      Screen.Set_Pixel ((Box_Right - 1, Box_Bottom - 1));

      if Index /= 0 then
         X := 0;
         Print (X_Offset => X,
                Y_Offset => Arrow_Y_Offset,
                C        => '<');
      end if;
   end Draw_Menu_Box;

   ---------------------
   -- Draw_Precentage --
   ---------------------

   procedure Draw_Precentage (Title : String;
                              Val : Percentage)
   is
      X : Integer := Box_Center.X - 50;
   begin
      Print (X_Offset    => X,
             Y_Offset    => 4 + Box_Top + 4,
             Str         => Title);

      Screen.Draw_Line ((Box_Center.X - 50, Box_Center.Y),
                        (Box_Center.X + 50, Box_Center.Y));

      Screen.Draw_Line ((Box_Center.X - 50 + Val, Box_Center.Y - 2),
                        (Box_Center.X - 50 + Val, Box_Center.Y + 2));

      X := Box_Center.X - 50;
      Print (X_Offset    => X,
             Y_Offset    => Value_Text_Y,
             Str         => Val'Img & "%");

   end Draw_Precentage;

   --------------
   -- Draw_Pan --
   --------------

   procedure Draw_Pan (Title : String;
                       Val : Pan)
   is
      X : Integer := Box_Center.X - 50;
   begin
      Print (X_Offset    => X,
             Y_Offset    => 4 + Box_Top + 4,
             Str         => Title);

      Screen.Draw_Line ((Box_Center.X - 50, Box_Center.Y),
                        (Box_Center.X + 50, Box_Center.Y));

      Screen.Draw_Line ((Box_Center.X + Val, Box_Center.Y - 2),
                        (Box_Center.X + Val, Box_Center.Y + 2));

      X := Box_Center.X - 50;
      Print (X_Offset    => X,
             Y_Offset    => Value_Text_Y,
             Str         => Val'Img);
   end Draw_Pan;

   -------------------
   -- Draw_MIDI_Val --
   -------------------

   procedure Draw_MIDI_Val (Title : String;
                            Val   : MIDI.MIDI_Data)
   is
      X : Integer := Box_Center.X - 50;
   begin
      Print (X_Offset    => X,
             Y_Offset    => 4 + Box_Top + 4,
             Str         => Title);

      X := Box_Center.X - 50;
      Print (X_Offset    => X,
             Y_Offset    => Value_Text_Y,
             Str         => Val'Img);
   end Draw_MIDI_Val;

   ---------------
   -- Draw_Text --
   ---------------

   procedure Draw_Text (Title : String;
                        Val   : String)
   is
      X : Integer := Box_Center.X - 50;
   begin
      Print (X_Offset    => X,
             Y_Offset    => 4 + Box_Top + 4,
             Str         => Title);

      X := Box_Center.X - 50;
      Print (X_Offset    => X,
             Y_Offset    => Value_Text_Y,
             Str         => Val);
   end Draw_Text;

   ---------------
   -- Draw_Knob --
   ---------------

   procedure Draw_Knob (Title : String;
                        Value : Natural)
   is
      pragma Unreferenced (Title, Value);
   begin
      Screen.Draw_Line ((Box_Left, Box_Center.Y),
                        (Box_Left + 100, Box_Center.Y));
      Screen.Draw_Circle (Box_Center, 15);
   end Draw_Knob;

end WNM.GUI.Menu.Drawing;
