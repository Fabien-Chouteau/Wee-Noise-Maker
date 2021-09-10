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
with WNM.Sequence_Copy;     use WNM.Sequence_Copy;
with WNM.Pattern_Sequencer; use WNM.Pattern_Sequencer;
with WNM.Master_Volume;
with WNM.GUI.Menu;
with WNM.GUI.Logo;
with WNM.GUI.Popup;
--  with WNM.Sample_Library;    use WNM.Sample_Library;

--  with Quick_Synth;           use Quick_Synth;

package body WNM.GUI.Update is

   Anim_Step : HAL.UInt32 := 0;

   Next_Start : Time.Time_Ms := Time.Time_Ms'First;

   function Header_Str return String is
      Pat : constant Keyboard_Value := Current_Pattern;
      Trk : constant Keyboard_Value := Track;
      Stp : constant Keyboard_Value := UI.Current_Editting_Trig;

      function To_Str (V : Keyboard_Value) return String
      is (case V is
             when 1 => "01",
             when 2 => "02",
             when 3 => "03",
             when 4 => "04",
             when 5 => "05",
             when 6 => "06",
             when 7 => "07",
             when 8 => "08",
             when 9 => "09",
             when 10 => "10",
             when 11 => "11",
             when 12 => "12",
             when 13 => "13",
             when 14 => "14",
             when 15 => "15",
             when 16 => "16"
         );

      Result : String (1 .. 11) := "P00:T00:S00";
   begin
      Result (2 .. 3) := To_Str (Pat);
      Result (6 .. 7) := To_Str (Trk);
      Result (10 .. 11) := To_Str (Stp);
      return Result;
   end Header_Str;

   ------------
   -- Update --
   ------------

   function Update return Time.Time_Ms is
      B : Integer;

      BPM : Natural;
      Volume : Natural;
      Now : constant Time.Time_Ms := Time.Clock;
   begin
      if Now < Next_Start then
         return Next_Start;
      end if;

      Next_Start := Next_Start + GUI_Task_Period_Ms;

      WNM.Screen.Clear;

      --  Splash screen
      if WNM.Time.Clock < 1000 then
         WNM.GUI.Logo.Draw_On_Screen (UInt2 (Anim_Step mod 4));
         WNM.Screen.Update;
         Anim_Step := Anim_Step + 1;
         return Next_Start;
      end if;

      -- Header --
      B := 1;
      Print (X_Offset    => B,
             Y_Offset    => 0,
             Str         => Header_Str);

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
                   Y_Offset    => 0 + 8,
                   Str         => "Select track");
            B := 1;
            Print (X_Offset    => B,
                   Y_Offset    => 9 + 8,
                   Str         => "Current: " & Img (Track));
         when WNM.UI.Pattern_Select =>
            B := 1;
            Print (X_Offset    => B,
                   Y_Offset    => 0 + 8,
                   Str         => "Chain patterns");
            B := 1;
            Print (X_Offset    => B,
                   Y_Offset    => 9 + 8,
                   Str         => "Current: " & Img (Current_Pattern));
         when WNM.UI.Note =>
            null;
         when WNM.UI.FX_Alt =>
            B := 1;
            Print (X_Offset    => B,
                   Y_Offset    => 0 + 8,
                   Str         => "FX/Copy",
                   Invert_From => 0);
         when WNM.UI.Copy =>
            declare
               Blink : constant Boolean := (Anim_Step mod 10) < 5;
               FB : constant String := (if Blink then "  " else "??");
               TB : constant String :=
                 (if Is_Complete (WNM.UI.Copy_T.From) and then blink
                  then "  " else "??");
            begin
               B := 1;
               Print (X_Offset    => B,
                      Y_Offset    => 0 + 8,
                      Str         => "Copy " & WNM.UI.Copy_T.From.Kind'Img);
               B := 1;
               Print (X_Offset    => B,
                      Y_Offset    => 8 + 8,
                      Str         => "From " & Image (WNM.UI.Copy_T.From, FB));
               B := 1;
               Print (X_Offset    => B,
                      Y_Offset    => 16 + 8,
                      Str         => "To   " & Image (WNM.UI.Copy_T.To, TB));
            end;
         when WNM.UI.Step_Edit =>
            null; --  Step edit is a menu
         end case;
      end if;

      WNM.GUI.Popup.Update;

      WNM.Screen.Update;

      Anim_Step := Anim_Step + 1;

      return Next_Start;
   end Update;

end WNM.GUI.Update;
