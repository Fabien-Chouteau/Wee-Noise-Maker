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

--  Hold record to enter sequencer mode. Move between steps with an encoder
--  The current step number is shown on the screen. Press a note to add/remove
--  it to/from the current step. Active notes for this step are shown with the
--  LEDs. Press the current chan to erase the squence.

with WNM.Sequencer;                use WNM.Sequencer;
with WNM.Encoders;                 use WNM.Encoders;
with WNM.Synth;                  use WNM.Synth;
with WNM.Master_Volume;
with WNM.Pattern_Sequencer;
with WNM.GUI.Menu;
with WNM.GUI.Menu.Root;
with WNM.GUI.Menu.Track_Settings;
with WNM.GUI.Menu.Step_Settings;
with WNM.Buttons;
with WNM.LED;
with HAL; use HAL;

package body WNM.UI is

   procedure Signal_Event (B : Button; Evt : Buttton_Event);

   procedure Toggle_FX (B : Keyboard_Button);

   Default_Input_Mode : constant Input_Mode_Type := Note;

   FX_Is_On : array (Tracks) of Boolean := (others => False);
   Current_Input_Mode : Input_Mode_Type := Note;

   Editting_Step : Sequencer_Steps := 1;

   ----------------
   -- Input_Mode --
   ----------------

   function Input_Mode return Input_Mode_Type
   is (Current_Input_Mode);

   ------------------
   -- Signal_Event --
   ------------------

   procedure Signal_Event (B : Button; Evt : Buttton_Event) is
   begin
      if GUI.Menu.In_Menu and then Evt = On_Press then
         if B = Encoder_L then
            GUI.Menu.On_Event ((Kind => GUI.Menu.Left_Press));
            return;
         end if;

         if B = Encoder_R then
            GUI.Menu.On_Event ((Kind => GUI.Menu.Right_Press));
            return;
         end if;
      end if;

      case Current_Input_Mode is
         when Note =>
            case Evt is
               when On_Press =>
                  case B is
                     when Func =>
                        --  Switch to Func mode
                        Current_Input_Mode := FX_Select;
                     when Play =>
                        Sequencer.Play_Pause;
                     when Rec =>
                        Sequencer.Rec_Pressed;
                     when Keyboard_Button =>
                        Sequencer.On_Press (B);
                     when Track_Button =>
                        Current_Input_Mode := Track_Select;
                     when Pattern =>
                        Current_Input_Mode := Pattern_Select;
                     when Menu =>
                        if not GUI.Menu.In_Menu then
                           GUI.Menu.Root.Push_Root_Window;
                        end if;
                     when others => null;
                  end case;
               when On_Long_Press =>
                  case B is
                     when Play =>
                        --  Switch to volume/BPM config mode
                        Current_Input_Mode := Volume_BPM;
                     when Rec =>
                        --  Switch to squence edition mode
                        Sequencer.Rec_Long;
                     when B1 .. B16 =>

                        GUI.Menu.Step_Settings.Push_Window;
                        Current_Input_Mode := Step_Edit;
                        Editting_Step := To_Value (B);


                     when others => null;
                  end case;
               when On_Release =>
                  case B is
                     when Keyboard_Button =>
                        --  Release note or octave Up/Down
                        Sequencer.On_Release (B);
                     when Rec =>
                        Sequencer.Rec_Release;
                     when others => null;
                  end case;
               when others => null;
            end case;

         when Volume_BPM =>
            if B = Play and Evt = On_Release then
               Current_Input_Mode := Default_Input_Mode;
               WNM.Pattern_Sequencer.End_Sequence_Edit;
            end if;

            if B in B1 .. B16 and Evt = On_Press then
               WNM.Synth.Toggle_Mute (B);
            end if;
         when FX_Select =>
            case Evt is
               when On_Press =>
                  case B is
                     when Keyboard_Button =>
                        Toggle_FX (B);
                     when others =>
                        null;
                  end case;
               when On_Release =>
                  if B = Func then
                     Current_Input_Mode := Default_Input_Mode;
                  end if;
               when others =>
                  null;
            end case;

         when Track_Select =>
            if B in B1 .. B16 and then Evt = On_Press then
               Sequencer.Select_Track (B);
            elsif B = Track_Button and then Evt = On_Release then
               Current_Input_Mode := Default_Input_Mode;
            end if;
         when Pattern_Select =>
            if B = Rec and then Evt in On_Press | On_Long_Press then
               Current_Input_Mode := Pattern_Copy;
            elsif B in B1 .. B16 and then Evt = On_Press then
               Pattern_Sequencer.Add_To_Sequence (B);
            elsif B = Pattern and then Evt = On_Release then
               Pattern_Sequencer.End_Sequence_Edit;
               Current_Input_Mode := Default_Input_Mode;
            end if;
         when Pattern_Copy =>
            if B = Rec and then Evt = On_Release then
               Current_Input_Mode := Pattern_Select;
            elsif B in B1 .. B16 and then Evt = On_Press then
               Sequencer.Copy_Current_Patern (To => B);
               Pattern_Sequencer.Add_To_Sequence (B);
            elsif B = Pattern and then Evt = On_Release then
               Pattern_Sequencer.End_Sequence_Edit;
               Current_Input_Mode := Default_Input_Mode;
            end if;
         when Step_Edit =>
            if B in B1 .. B16 and then Evt = On_Press then
               Editting_Step := To_Value (B);
            end if;

            if not WNM.GUI.Menu.In_Menu then
               Current_Input_Mode := Default_Input_Mode;
            end if;
      end case;
   end Signal_Event;

   ---------------
   -- Toggle_FX --
   ---------------

   procedure Toggle_FX (B : Tracks) is
   begin
      FX_Is_On (B) := not FX_Is_On (B);
   end Toggle_FX;

   ---------------------------
   -- Current_Editting_Trig --
   ---------------------------

   function Current_Editting_Trig return Sequencer_Steps
   is (Editting_Step);

   -----------
   -- FX_On --
   -----------

   function FX_On (B : Tracks) return Boolean
   is (FX_Is_On (B));

   --------------------
   -- Has_Long_Press --
   --------------------

   function Has_Long_Press (B : Button) return Boolean is

      In_Edit : constant Boolean := Sequencer.State in Play_And_Edit | Edit;

      In_Pattern_Select : constant Boolean :=
        Current_Input_Mode in Pattern_Select | Pattern_Copy;
   begin
      return (case B is
              when B1        => In_Edit,
              when B2        => In_Edit,
              when B3        => In_Edit,
              when B4        => In_Edit,
              when B5        => In_Edit,
              when B6        => In_Edit,
              when B7        => In_Edit,
              when B8        => In_Edit,
              when B9        => In_Edit,
              when B10       => In_Edit,
              when B11       => In_Edit,
              when B12       => In_Edit,
              when B13       => In_Edit,
              when B14       => In_Edit,
              when B15       => In_Edit,
              when B16       => In_Edit,
              when Rec       => not In_Pattern_Select,
              when Play      => True,
              when Func      => False,
              when Track_Button     => False,
              when Pattern   => False,
              when Menu      => False,
              when Encoder_L => True,
              when Encoder_R => True);
   end Has_Long_Press;


   Last_State    : array (Button) of Buttons.Raw_Button_State := (others => WNM.Buttons.Up);
   Pressed_Since : array (Button) of WNM.Time.Time_Ms := (others => 0);
   Last_Event    : array (Button) of Buttton_Event := (others => On_Release);
   Next_Start    : Time.Time_Ms := Time.Time_Ms'First;

   ------------
   -- Update --
   ------------

   function Update return Time.Time_Ms is
      use Buttons;

      L_Enco : Integer;
      R_Enco : Integer;

      Now : constant Time.Time_Ms := Time.Clock;
   begin
      if Now < Next_Start then
         return Next_Start;
      end if;

      Next_Start := Next_Start + UI_Task_Period_Ms;

      Buttons.Scan;

      --  Handle buttons
      for B in Button loop
         if Last_State (B) = State (B) then
            --  The button didn't change, let's check if we are waiting for
            --  a long press event.
            if Has_Long_Press (B)
              and then
                State (B) = Down
              and then
                Last_Event (B) = Waiting_For_Long_Press
              and then
                Pressed_Since (B) + Long_Press_Time_Span_Ms < Now
            then
               Last_Event (B) := On_Long_Press;
               Signal_Event (B, Last_Event (B));
            end if;

         elsif State (B) = Down then
            --  Button was justed pressed

            if Has_Long_Press (B) then
               --  If this button has long press event we don't signal the
               --  On_Press right now, but we record the time at wich it was
               --  pressed.

               Last_Event (B) := Waiting_For_Long_Press;
               Pressed_Since (B) := Now;
            else
               Last_Event (B) := On_Press;
               Signal_Event (B, Last_Event (B));
            end if;
         else
            --  Button was just released

            if Last_Event (B) = Waiting_For_Long_Press then
               --  The button was released before we reached the long press
               --  delay. It was not a long press after all so we first send
               --  The On_Press event and then the On_Realease.
               Signal_Event (B, On_Press);
            end if;

            Last_Event (B) := On_Release;
            Signal_Event (B, Last_Event (B));
         end if;

         Last_State (B) := State (B);
      end loop;

      --------------
      -- Encoders --
      --------------

      L_Enco := WNM.Buttons.Left_Diff;
      R_Enco := WNM.Buttons.Right_Diff;

      if GUI.Menu.In_Menu then
         if L_Enco /= 0 then
            GUI.Menu.On_Event ((Kind  => GUI.Menu.Encoder_Left,
                                Value => L_Enco));
         end if;
         if R_Enco /= 0 then
            GUI.Menu.On_Event ((Kind  => GUI.Menu.Encoder_Right,
                                Value => R_Enco));
         end if;
      else
         case Current_Input_Mode is
            when Volume_BPM =>
               WNM.Sequencer.Change_BPM (R_Enco);
               WNM.Master_Volume.Change (L_Enco);
            when Track_Select =>
               null;
               --  Quick_Synth.Change_Pan (Sequencer.Track, R_Enco);
               --  Quick_Synth.Change_Volume (Sequencer.Track, L_Enco);
            when others =>
               if L_Enco /= 0 or else R_Enco /= 0 then
                  GUI.Menu.Track_Settings.Push_Window;
               end if;
         end case;
      end if;

      --------------
      -- Set LEDs --
      --------------

      LED.Turn_Off_All;

      -- Play LED --
      if Sequencer.State not in Pause | Edit then
         LED.Turn_On (Play);
         if Sequencer.Step in 1 | 5 | 9 | 13 then
            LED.Turn_On (Play);
         end if;
      end if;

      -- Rec LED --
      if Sequencer.State = Edit
        or else
          Sequencer.State in Play_And_Rec | Play_And_Edit
      then
         LED.Turn_On (Rec);
      end if;

      --  B1 .. B16 LEDs --
      case Current_Input_Mode is

         -- FX selection mode --
         when FX_Select =>
            --  The FX LED will be on if there's at least one FX enabled

            for B in B1 .. B16 loop
               if FX_Is_On (B) then
                  LED.Turn_On (B);
               end if;
            end loop;

            -- Track assign mode --
         when Track_Select =>
            for B in B1 .. B16 loop
               if Sequencer.Track = B then
                  LED.Turn_On (B);
               end if;
            end loop;

            --  Pattern select --
         when Pattern_Select | Pattern_Copy =>
            for B in B1 .. B16 loop
               if Pattern_Sequencer.Current_Pattern = B then
                  LED.Turn_On (B);
               end if;
               if Pattern_Sequencer.Is_In_Pattern_Sequence (B) then
                  LED.Turn_On (B);
               end if;
            end loop;

         --  Volume and BPM mode --
         when Volume_BPM =>
            for B in B1 .. B16 loop
               if not WNM.Synth.Muted (B) then
                  LED.Turn_On (B);
               end if;
            end loop;

            --  Any other mode --
         when others =>
            case Sequencer.State is
               when Edit | Play_And_Edit =>
                  for B in B1 .. B16 loop
                     if Sequencer.Set (To_Value (B)) then
                        LED.Turn_On (B);
                     end if;
                  end loop;
               when Play | Play_And_Rec =>
                  for B in B1 .. B16 loop
                     if Sequencer.Set (B, Sequencer.Step) then
                        LED.Turn_On (B);
                     end if;
                  end loop;
               when others =>
                  null;
            end case;
      end case;

      if Sequencer.State in Play_And_Edit | Play_And_Rec | Play then
         LED.Turn_On (To_Button (Sequencer.Step));
      end if;

      return Next_Start;
   end Update;

end WNM.UI;
