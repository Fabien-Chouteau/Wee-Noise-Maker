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

with HAL;                  use HAL;
with HAL.Bitmap;
with WNM.GUI.Bitmap_Fonts;
with WNM.GUI.Logo;
with WNM.Screen;
with WNM.UI;
with WNM.Sequencer;
with WNM.Master_Volume;

package body WNM.GUI is

   Anim_Step : HAL.UInt32 := 0;

   ------------
   -- Update --
   ------------

   procedure Update is
      B : Integer;

      BPM : Natural;
      Volume : Natural;
   begin
      WNM.Screen.Buffer.Set_Source (HAL.Bitmap.Black);
      WNM.Screen.Buffer.Fill;

      case WNM.UI.Input_Mode is
         when WNM.UI.Volume_BPM =>
            WNM.Screen.Buffer.Set_Source (HAL.Bitmap.White);
            BPM := Integer (WNM.Sequencer.BPM);
            Volume := Integer (WNM.Master_Volume.Value);
            B := 1;
            WNM.GUI.Bitmap_Fonts.Print (Buffer      => WNM.Screen.Buffer.all,
                                        X_Offset    => B,
                                        Y_Offset    => 0,
                                        Str         => String'(" Volume:" & Volume'Img & "%       "),
                                        Invert_From => Volume mod 96);
            B := 1;
            WNM.GUI.Bitmap_Fonts.Print (Buffer   => WNM.Screen.Buffer.all,
                                        X_Offset => B,
                                        Y_Offset => 8,
                                        Str      => String'(" BPM:" & BPM'Img),
                                        Invert_From => BPM mod 96);
         when WNM.UI.Track_Assign =>
            WNM.Screen.Buffer.Set_Source (HAL.Bitmap.White);
            B := 1;
            WNM.GUI.Bitmap_Fonts.Print (Buffer      => WNM.Screen.Buffer.all,
                                        X_Offset    => B,
                                        Y_Offset    => 0,
                                        Str         => String'("Assign instr."),
                                        Invert_From => 0);
            B := 1;
            WNM.GUI.Bitmap_Fonts.Print (Buffer      => WNM.Screen.Buffer.all,
                                        X_Offset    => B,
                                        Y_Offset    => 8,
                                        Str         => String'("to " & WNM.Sequencer.Track'Img),
                                        Invert_From => 0);
         when WNM.UI.Note =>
            WNM.GUI.Logo.Draw_On_Screen (HAL.UInt2 (Anim_Step mod 4));
         when WNM.UI.FX_Select =>
            WNM.Screen.Buffer.Set_Source (HAL.Bitmap.White);
            B := 1;
            WNM.GUI.Bitmap_Fonts.Print (Buffer      => WNM.Screen.Buffer.all,
                                        X_Offset    => B,
                                        Y_Offset    => 0,
                                        Str         => String'("Enable FX"),
                                        Invert_From => 0);
         when WNM.UI.Sequence_Edit =>
            WNM.Screen.Buffer.Set_Source (HAL.Bitmap.White);
            B := 1;
            WNM.GUI.Bitmap_Fonts.Print (Buffer      => WNM.Screen.Buffer.all,
                                        X_Offset    => B,
                                        Y_Offset    => 0,
                                        Str         => String'("Edit sequence"),
                                        Invert_From => 0);
      end case;
      WNM.Screen.Update;

      Anim_Step := Anim_Step + 1;
   end Update;

end WNM.GUI;
