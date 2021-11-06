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
with WNM.GUI.Menu.Drawing; use WNM.GUI.Menu.Drawing;

with WNM.Chord_Sequencer;

package body WNM.GUI.Menu.Pattern_Settings is

   package Top_Settings_Next is new Enum_Next (Top_Settings);
   use Top_Settings_Next;
   package Sub_Settings_Next is new Enum_Next (Sub_Settings);
   use Sub_Settings_Next;

   Pattern_Menu_Singleton : aliased Pattern_Settings_Menu;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Pattern_Menu_Singleton'Access);
   end Push_Window;

   ------------
   -- To_Top --
   ------------

   function To_Top (S : Sub_Settings) return Top_Settings
   is (case S is
          when Scale_Key => Scale,
          when Scale_Mode => Scale,
          when Chord_Index => Chord_Index);

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw (This : in out Pattern_Settings_Menu)
   is
      use Chord_Sequencer;
      Top_Setting : constant Top_Settings := To_Top (This.Current_Setting);
   begin
      Draw_Menu_Box ("Pattern settings",
                     Count => Top_Settings_Count,
                     Index => Top_Settings'Pos (To_Top (This.Current_Setting)));

      case Top_Setting is
         when Scale =>
            Draw_Text ("Key", "");
            Draw_MIDI_Note (Current_Scale_Key,
                            This.Current_Setting = Scale_Key);
            Draw_Scale_Mode (Chord_Sequencer.Current_Scale,
                             This.Current_Setting = Scale_Mode);
         when Chord_Index =>
            Draw_Text ("Chord Index",
                       Current_Chord_Index'Img);
      end case;

   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event (This  : in out Pattern_Settings_Menu;
                       Event : Menu_Event)
   is
   begin
      case Event.Kind is
         when Left_Press =>
            null;
         when Right_Press =>
            --  Never exit the step settings
            null;
         when Encoder_Right =>
            case This.Current_Setting is
               when Scale_Key =>
                  if Event.Value > 0 then
                     Chord_Sequencer.Scale_Key_Next;
                  else
                     Chord_Sequencer.Scale_Key_Prev;
                  end if;
               when Scale_Mode =>
                  if Event.Value > 0 then
                     Chord_Sequencer.Scale_Next;
                  else
                     Chord_Sequencer.Scale_Prev;
                  end if;

               when Chord_Index =>
                  if Event.Value > 0 then
                     Chord_Sequencer.Chord_Index_Next;
                  else
                     Chord_Sequencer.Chord_Index_Prev;
                  end if;
            end case;

         when Encoder_Left =>
            if Event.Value > 0 then
               This.Current_Setting := Next (This.Current_Setting);
            elsif Event.Value < 0 then
               This.Current_Setting := Prev (This.Current_Setting);
            end if;
      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding
   procedure On_Pushed (This  : in out Pattern_Settings_Menu)
   is
   begin
      null;
   end On_Pushed;

   --------------
   -- On_Focus --
   --------------

   overriding
   procedure On_Focus (This       : in out Pattern_Settings_Menu;
                       Exit_Value : Window_Exit_Value)
   is
   begin
      null;
   end On_Focus;

end WNM.GUI.Menu.Pattern_Settings;
