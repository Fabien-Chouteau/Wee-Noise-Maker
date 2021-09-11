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
with WNM.Screen;

package body WNM.GUI.Menu.Drawing is

   Title_Y_Offset : constant := 10;
   Box_Y_Offset : constant := 19;

   -------------------
   -- Draw_Menu_Box --
   -------------------

   procedure Draw_Menu_Box
     (Title       : String;
      Text        : String;
      Top, Bottom : Boolean)
   is
      X : Integer;
   begin

      X := (Screen.Width - Title'Length * Bitmap_Fonts.Width) / 2;
      Print (X_Offset    => X,
             Y_Offset    => Title_Y_Offset,
             Str         => Title);

      Screen.Draw_Dot_H_Line (Box_Y_Offset - 1);

      if Top then
         Screen.Set_Pixel ((0, 0 + Box_Y_Offset));
         Screen.Set_Pixel ((1, 1 + Box_Y_Offset));

         Screen.Set_Pixel ((Screen.Width - 1, 0 + Box_Y_Offset));
         Screen.Set_Pixel ((Screen.Width - 2, 1 + Box_Y_Offset));
      end if;

      --  Top line
      Screen.Draw_Line (Start => (2, 2 + Box_Y_Offset),
                        Stop  => (Screen.Width - 3, 2 + Box_Y_Offset));
      --  Bottom line
      Screen.Draw_Line (Start => (2, 12 + Box_Y_Offset),
                        Stop  => (Screen.Width - 3, 12 + Box_Y_Offset));

      --  Side left
      Screen.Draw_Line (Start => (0, 4 + Box_Y_Offset),
                        Stop  => (0, 10 + Box_Y_Offset));
      --  Side right
      Screen.Draw_Line (Start => (Screen.Width - 1, 4 + Box_Y_Offset),
                        Stop  => (Screen.Width - 1, 10 + Box_Y_Offset));

      Screen.Set_Pixel ((1, 3 + Box_Y_Offset));
      Screen.Set_Pixel ((1, 11 + Box_Y_Offset));
      Screen.Set_Pixel ((Screen.Width - 2, 3 + Box_Y_Offset));
      Screen.Set_Pixel ((Screen.Width - 2, 11 + Box_Y_Offset));

      X := 3;
      Print (X_Offset    => X,
             Y_Offset    => 4 + Box_Y_Offset,
             Str         => Text);

      if Bottom then
         Screen.Set_Pixel ((0, 15 + Box_Y_Offset));
         Screen.Set_Pixel ((0, 14 + Box_Y_Offset));
         Screen.Set_Pixel ((1, 13 + Box_Y_Offset));

         Screen.Set_Pixel ((Screen.Width - 2, 13 + Box_Y_Offset));
         Screen.Set_Pixel ((Screen.Width - 1, 14 + Box_Y_Offset));
         Screen.Set_Pixel ((Screen.Width - 1, 15 + Box_Y_Offset));
      end if;
   end Draw_Menu_Box;

end WNM.GUI.Menu.Drawing;
