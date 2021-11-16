-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2020 Fabien Chouteau                    --
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
with WNM.Sequencer;        use WNM.Sequencer;
with WNM.UI;

package body WNM.GUI.Menu.Step_Settings is

   package Top_Settings_Next is new Enum_Next (Top_Settings,
                                               Wrap => False);
   use Top_Settings_Next;
   package Sub_Settings_Next is new Enum_Next (Sub_Settings,
                                               Wrap => False);
   use Sub_Settings_Next;

   Step_Settings_Singleton : aliased Step_Settings_Menu;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Step_Settings_Singleton'Access);
   end Push_Window;

   ------------
   -- To_Top --
   ------------

   function To_Top (S : Sub_Settings) return Top_Settings
   is (case S is
          when Condition => Condition,
          when Note => Note,
          when Duration => Note,
          when Velo => Note,
          when Repeat => Repeat,
          when Repeat_Rate => Repeat,
          when CC_A => CC_A,
          when CC_B => CC_B,
          when CC_C => CC_C,
          when CC_D => CC_D);

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw
     (This : in out Step_Settings_Menu)
   is

      Trig : constant Sequencer_Steps := Sequencer.Editing_Step;

      Top_Setting : constant Top_Settings := To_Top (This.Current_Setting);
   begin
      Draw_Menu_Box ("Step settings",
                     Count => Top_Settings_Count,
                     Index => Top_Settings'Pos (To_Top (This.Current_Setting)));

      case Top_Setting is
         when Condition =>
            Draw_Title ("Condition", "");
            Draw_Value (Img (Sequencer.Trig (Trig)));

         when Note      =>
            case This.Current_Setting is
               when Note =>
                  Draw_Title (Img (Sequencer.Note_Mode (Trig)), "");
               when Duration =>
                  Draw_Title ("Duration", "");
               when Velo =>
                  Draw_Title ("Velocity", "");
               when others =>
                  raise Program_Error;
            end case;

            case Sequencer.Note_Mode (Trig) is
               when Note | Note_In_Chord | Note_In_Scale =>
                  Draw_MIDI_Note (Sequencer.Note (Trig),
                                  This.Current_Setting = Note);
               when Chord =>
                  Draw_Value (Sequencer.Note (Trig)'Img);

               when Arp =>
                  Draw_Value ("---");
            end case;

            Draw_Duration (Sequencer.Duration (Trig),
                           This.Current_Setting = Duration);

            Draw_MIDI_Val (Sequencer.Velo (Trig),
                           This.Current_Setting = Velo);

         when Repeat =>
            case This.Current_Setting is
               when Repeat =>
                  Draw_Title ("Repeat Count", "");
                  Draw_Value (Sequencer.Repeat (Trig)'Img);
               when Repeat_Rate =>
                  Draw_Title ("Repeat Rate", "");
                  Draw_Value (Img (Sequencer.Repeat_Rate (Trig)));
               when others =>
                  raise Program_Error;
            end case;

         when CC_A .. CC_D =>
            declare
               Id : constant Sequencer.CC_Id :=
                 (case This.Current_Setting is
                  when CC_A => Sequencer.A,
                  when CC_B => Sequencer.B,
                  when CC_C => Sequencer.C,
                  when others => Sequencer.D);
            begin
               Draw_Title (Sequencer.CC_Controller_Label (Editing_Track, Id),
                           "");

               if Sequencer.CC_Enabled (Trig, Id) then
                  Draw_MIDI_Val (Sequencer.CC_Value (Trig, Id),
                                Selected => False);
               else
                  Draw_Value ("- Disabled -");
               end if;
            end;
      end case;
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event
     (This  : in out Step_Settings_Menu;
      Event : Menu_Event)
   is
      Trig : constant Sequencer_Steps := Sequencer.Editing_Step;
   begin
      case Event.Kind is
         when Left_Press =>
            case This.Current_Setting is
               when CC_A => Sequencer.CC_Toggle (Trig, A);
               when CC_B => Sequencer.CC_Toggle (Trig, B);
               when CC_C => Sequencer.CC_Toggle (Trig, C);
               when CC_D => Sequencer.CC_Toggle (Trig, D);

               when Note => Sequencer.Note_Mode_Next (Trig);

               when others => null;
            end case;
         when Right_Press =>
            --  Never exit the step settings
            null;
         when Encoder_Right =>
            case This.Current_Setting is
               when Condition =>
                  if Event.Value > 0 then
                     WNM.Sequencer.Trig_Next (Trig);
                  else
                     WNM.Sequencer.Trig_Prev (Trig);
                  end if;
               when Note =>
                  if Event.Value > 0 then
                     WNM.Sequencer.Note_Next (Trig);
                  else
                     WNM.Sequencer.Note_Prev (Trig);
                  end if;
               when Duration =>
                  if Event.Value > 0 then
                     WNM.Sequencer.Duration_Next (Trig);
                  else
                     WNM.Sequencer.Duration_Prev (Trig);
                  end if;
               when Repeat =>
                  if Event.Value > 0 then
                     WNM.Sequencer.Repeat_Next (Trig);
                  else
                     WNM.Sequencer.Repeat_Prev (Trig);
                  end if;
               when Repeat_Rate =>
                  if Event.Value > 0 then
                     WNM.Sequencer.Repeat_Rate_Next (Trig);
                  else
                     WNM.Sequencer.Repeat_Rate_Prev (Trig);
                  end if;
               when Velo =>
                  if Event.Value > 0 then
                     WNM.Sequencer.Velo_Next (Trig);
                  else
                     WNM.Sequencer.Velo_Prev (Trig);
                  end if;
               when CC_A | CC_B | CC_C | CC_D =>
                  if Event.Value > 0 then
                     WNM.Sequencer.CC_Value_Inc (Trig,
                                                 (case This.Current_Setting is
                                                     when CC_A => A,
                                                     when CC_B => B,
                                                     when CC_C => C,
                                                     when others => D));
                  else
                     WNM.Sequencer.CC_Value_Dec (Trig,
                                                 (case This.Current_Setting is
                                                     when CC_A => A,
                                                     when CC_B => B,
                                                     when CC_C => C,
                                                     when others => D));
                  end if;
            end case;
         when Encoder_Left =>
            if Event.Value > 0 then
               Next (This.Current_Setting);
            elsif Event.Value < 0 then
               Prev (This.Current_Setting);
            end if;
      end case;
   end On_Event;

      ---------------
   -- On_Pushed --
   ---------------

   overriding procedure On_Pushed
     (This  : in out Step_Settings_Menu)
   is
   begin
      null;
   end On_Pushed;

   --------------
   -- On_Focus --
   --------------

   overriding procedure On_Focus
     (This       : in out Step_Settings_Menu;
      Exit_Value : Window_Exit_Value)
   is
   begin
      null;
   end On_Focus;

end WNM.GUI.Menu.Step_Settings;
