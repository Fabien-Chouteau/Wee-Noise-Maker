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

with HAL;                   use HAL;
with HAL.Bitmap;
with WNM.GUI.Bitmap_Fonts;  use WNM.GUI.Bitmap_Fonts;
with WNM.GUI.Parameters;
with WNM.Screen;
with WNM.UI;
with WNM.Sequencer;         use WNM.Sequencer;
with WNM.Pattern_Sequencer; use WNM.Pattern_Sequencer;
with WNM.Master_Volume;
with WNM.GUI.Menu;
with WNM.GUI.Logo;
with WNM.Time;
--  with WNM.Sample_Library;    use WNM.Sample_Library;

--  with Quick_Synth;           use Quick_Synth;

package body WNM.GUI.Update is

   Anim_Step : HAL.UInt32 := 0;

   ------------
   -- Update --
   ------------

   procedure Update is
      B : Integer;

      BPM : Natural;
      Volume : Natural;
   begin
      WNM.Screen.Clear;

      --  Splash screen
      if WNM.Time.Clock < 1000 then
         WNM.GUI.Logo.Draw_On_Screen (UInt2 (Anim_Step mod 4));
         WNM.Screen.Update;
         Anim_Step := Anim_Step + 1;
         return;
      end if;

      if Menu.In_Menu then
         Menu.Draw;
      else
         case WNM.UI.Input_Mode is
         when WNM.UI.Volume_BPM =>
            BPM := Integer (WNM.Sequencer.BPM);
            Volume := Integer (WNM.Master_Volume.Value);

            WNM.GUI.Parameters.Print_Percentage (Slot  => WNM.GUI.Parameters.Up,
                                                 Name  => "Volume",
                                                 Value => Volume);
            WNM.GUI.Parameters.Print_Int (Slot  => WNM.GUI.Parameters.Down,
                                          Name  => "BPM",
                                          Value => BPM,
                                          Min   => 50,
                                          Max   => 200);
         when WNM.UI.Track_Select =>
            B := 1;
            Print (X_Offset    => B,
                   Y_Offset    => 0,
                   Str         => "Select track");
            B := 1;
            Print (X_Offset    => B,
                   Y_Offset    => 9,
                   Str         => "Current:" & To_Value (Track)'Img);
         when WNM.UI.Pattern_Select =>
            B := 1;
            Print (X_Offset    => B,
                   Y_Offset    => 0,
                   Str         => "Chain patterns");
            B := 1;
            Print (X_Offset    => B,
                   Y_Offset    => 9,
                   Str         => "Current:" & To_Value (Current_Pattern)'Img);
         when WNM.UI.Pattern_Copy =>
            B := 1;
            Print (X_Offset    => B,
                   Y_Offset    => 0,
                   Str         => "Copy pattern");
         when WNM.UI.Note =>
            B := 1;
            Print (X_Offset    => B,
                   Y_Offset    => 0,
                   Str         => "Trk:" & To_Value (Track)'Img);
            B := 54;
            Print (X_Offset    => B,
                   Y_Offset    => 0,
                   Str         => "Pat:" & To_Value (Current_Pattern)'Img);
            B := 1;
            --  Print (X_Offset    => B,
            --         Y_Offset    => 9,
            --         Str         => Entry_Name (Sample_Of_Track (Track)));
         when WNM.UI.FX_Select =>
            B := 1;
            Print (X_Offset    => B,
                   Y_Offset    => 0,
                   Str         => "Enable FX",
                   Invert_From => 0);
         when WNM.UI.Trig_Edit =>
            B := 1;
            Print (X_Offset    => B,
                   Y_Offset    => 0,
                   Str         => Sequencer.Trig (UI.Current_Editting_Trig)'Img,
                   Invert_From => 0);
         end case;
      end if;

      WNM.Screen.Update;

      Anim_Step := Anim_Step + 1;
   end Update;

end WNM.GUI.Update;
