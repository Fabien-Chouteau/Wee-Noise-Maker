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

with HAL.Bitmap;           use HAL.Bitmap;
with WNM.GUI.Bitmap_Fonts; use WNM.GUI.Bitmap_Fonts;

package body WNM.GUI.Menu.Drawing is

   -------------------
   -- Draw_Menu_Box --
   -------------------

   procedure Draw_Menu_Box
     (Screen      : not null HAL.Bitmap.Any_Bitmap_Buffer;
      Text        : String;
      Top, Bottom : Boolean)
   is
      X : Integer := 3;
   begin
      Screen.Set_Source (Black);
      Screen.Fill;

      Screen.Set_Source (White);
      if Top then
         Screen.Set_Pixel ((0, 0));
         Screen.Set_Pixel ((1, 1));

         Screen.Set_Pixel ((95, 0));
         Screen.Set_Pixel ((94, 1));
      end if;

      --  Top line
      Screen.Draw_Line (Start     => (2, 2),
                        Stop      => (93, 2),
                        Thickness => 1,
                        Fast      => True);
      --  Bottom line
      Screen.Draw_Line (Start     => (2, 12),
                        Stop      => (93, 12),
                        Thickness => 1,
                        Fast      => True);

      --  Side left
      Screen.Draw_Line (Start     => (0, 4),
                        Stop      => (0, 10),
                        Thickness => 1,
                        Fast      => True);
      --  Side right
      Screen.Draw_Line (Start     => (95, 4),
                        Stop      => (95, 10),
                        Thickness => 1,
                        Fast      => True);

      Screen.Set_Pixel ((1, 3));
      Screen.Set_Pixel ((1, 11));
      Screen.Set_Pixel ((94, 3));
      Screen.Set_Pixel ((94, 11));

      Print (Buffer      => Screen.all,
             X_Offset    => X,
             Y_Offset    => 3,
             Str         => Text);

      if Bottom then
         Screen.Set_Pixel ((0, 15));
         Screen.Set_Pixel ((0, 14));
         Screen.Set_Pixel ((1, 13));

         Screen.Set_Pixel ((94, 13));
         Screen.Set_Pixel ((95, 14));
         Screen.Set_Pixel ((95, 15));
      end if;
   end Draw_Menu_Box;

end WNM.GUI.Menu.Drawing;
