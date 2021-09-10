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

with WNM.Screen;      use WNM.Screen;
with wnm_logo_text;
with wnm_logo_wave_1;
with wnm_logo_wave_2;
with wnm_logo_wave_3;
with wnm_logo_wave_4;

package body WNM.GUI.Logo is

   --------------------
   -- Draw_On_Screen --
   --------------------

   procedure Draw_On_Screen (Anim_Step : HAL.UInt2) is
   begin
      WNM.Screen.Clear;
      Copy_Bitmap (wnm_logo_text.Data, 6, 2 + 8);

      WNM.Screen.Draw_Line (Start     => (6, 11 + 8),
                            Stop      => (32, 11 + 8));
      WNM.Screen.Draw_Line (Start     => (63, 11 + 8),
                            Stop      => (89, 11 + 8));

      Copy_Bitmap ((case Anim_Step is
                      when 0 => wnm_logo_wave_1.Data,
                      when 1 => wnm_logo_wave_2.Data,
                      when 2 => wnm_logo_wave_3.Data,
                      when 3 => wnm_logo_wave_4.Data),
                   33, 9 + 8);
   end Draw_On_Screen;

end WNM.GUI.Logo;
