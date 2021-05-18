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

   Step_Settings_Singleton : aliased Step_Settings_Menu;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Step_Settings_Singleton'Access);
   end Push_Window;

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw
     (This : in out Step_Settings_Menu)
   is
      function CC_Letter (ID : Sequencer.CC_Id) return String
      is (case ID is
             when A => "A",
             when B => "B",
             when C => "C",
             when D => "D");

      function CC_String (Id : Sequencer.CC_Id) return String
      is (if Sequencer.CC_Enabled (UI.Current_Editting_Trig, Id)
          then CC_Letter (Id) & Sequencer.CC_Value (UI.Current_Editting_Trig,
                                                    Id)'Img
          else CC_Letter (Id) & "- disabled -");
   begin
      Draw_Menu_Box ((case This.Current_Setting is
                        when Condition   => Img (Sequencer.Trig (UI.Current_Editting_Trig)),
                        when Note        => "Note:" & Sequencer.Note (UI.Current_Editting_Trig)'Img,
                        when Repeat      => "Repeat:" & Sequencer.Repeat (UI.Current_Editting_Trig)'Img,
                        when Repeat_Rate => "Rate:" & Img (Sequencer.Repeat_Rate (UI.Current_Editting_Trig)),
                        when Velo        => "Velocity: " & Sequencer.Velo (UI.Current_Editting_Trig)'Img,
                        when CC_A        => CC_String (A),
                        when CC_B        => CC_String (B),
                        when CC_C        => CC_String (C),
                        when CC_D        => CC_String (D)),
                     Top => This.Current_Setting /= Settings'First,
                     Bottom => This.Current_Setting /= Settings'Last);
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding procedure On_Event
     (This  : in out Step_Settings_Menu;
      Event : Menu_Event)
   is
   begin
      case Event.Kind is
         when Left_Press =>
            case This.Current_Setting is
               when CC_A => Sequencer.CC_Toggle (UI.Current_Editting_Trig, A);
               when CC_B => Sequencer.CC_Toggle (UI.Current_Editting_Trig, B);
               when CC_C => Sequencer.CC_Toggle (UI.Current_Editting_Trig, C);
               when CC_D => Sequencer.CC_Toggle (UI.Current_Editting_Trig, D);
               when others => null;
            end case;
         when Right_Press =>
            Menu.Exit_Menu;
         when Encoder_Right =>
            case This.Current_Setting is
               when Condition =>
                  if Event.Value > 0 then
                     WNM.Sequencer.Trig_Next (UI.Current_Editting_Trig);
                  else
                     WNM.Sequencer.Trig_Prev (UI.Current_Editting_Trig);
                  end if;
               when Note =>
                  if Event.Value > 0 then
                     WNM.Sequencer.Note_Next (UI.Current_Editting_Trig);
                  else
                     WNM.Sequencer.Note_Prev (UI.Current_Editting_Trig);
                  end if;
               when Repeat =>
                  if Event.Value > 0 then
                     WNM.Sequencer.Repeat_Next (UI.Current_Editting_Trig);
                  else
                     WNM.Sequencer.Repeat_Prev (UI.Current_Editting_Trig);
                  end if;
               when Repeat_Rate =>
                  if Event.Value > 0 then
                     WNM.Sequencer.Repeat_Rate_Next (UI.Current_Editting_Trig);
                  else
                     WNM.Sequencer.Repeat_Rate_Prev (UI.Current_Editting_Trig);
                  end if;
               when Velo =>
                  if Event.Value > 0 then
                     WNM.Sequencer.Velo_Next (UI.Current_Editting_Trig);
                  else
                     WNM.Sequencer.Velo_Prev (UI.Current_Editting_Trig);
                  end if;
               when CC_A | CC_B | CC_C | CC_D =>
                  if Event.Value > 0 then
                     WNM.Sequencer.CC_Value_Inc (UI.Current_Editting_Trig,
                                                 (case This.Current_Setting is
                                                     when CC_A => A,
                                                     when CC_B => B,
                                                     when CC_C => C,
                                                     when others => D));
                  else
                     WNM.Sequencer.CC_Value_Dec (UI.Current_Editting_Trig,
                                                 (case This.Current_Setting is
                                                     when CC_A => A,
                                                     when CC_B => B,
                                                     when CC_C => C,
                                                     when others => D));
                  end if;
            end case;
         when Encoder_Left =>
            if Event.Value > 0 then
               if This.Current_Setting /= Settings'Last then
                  This.Current_Setting := Settings'Succ (This.Current_Setting);
               end if;
            elsif Event.Value < 0 then
               if This.Current_Setting /= Settings'First then
                  This.Current_Setting := Settings'Pred (This.Current_Setting);
               end if;
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
