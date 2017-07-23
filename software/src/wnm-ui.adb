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

with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;
with WNM.Sequencer;                use WNM.Sequencer;
with WNM.Encoders;                 use WNM.Encoders;
with Quick_Synth;                  use Quick_Synth;
with WNM.Master_Volume;

package body WNM.UI is

   UI_Task_Start   : Suspension_Object;

   procedure Turn_On (LED_Addr : LED_Address);
   procedure Turn_Off (LED_Addr : LED_Address);
   procedure Signal_Event (B : Buttons; Evt : Buttton_Event);

   procedure Set_FX (B : Buttons);

   Default_Input_Mode : constant Input_Mode_Type := Note;

   FX_Is_On : array (Buttons) of Boolean := (others => False);
   Current_Input_Mode : Input_Mode_Type := Note;

   ----------------
   -- Input_Mode --
   ----------------

   function Input_Mode return Input_Mode_Type
   is (Current_Input_Mode);

   ------------------
   -- Signal_Event --
   ------------------

   procedure Signal_Event (B : Buttons; Evt : Buttton_Event) is
   begin
      case Current_Input_Mode is
         when Note =>
            case Evt is
               when On_Press =>
                  case B is
                     when FX =>
                        --  Switch to FX mode
                        Current_Input_Mode := FX_Select;
                     when Play =>
                        Sequencer.Play_Pause;
                     when Rec =>
                        Sequencer.Rec;
                     when Track_A .. Track_E =>
                        --  Select current track
                        Sequencer.Select_Track (To_Track (B));
                     when Keyboard_Buttons =>
                        --  Play note or octave Up/Down
                        Sequencer.On_Press (B);
                     when others => null;
                  end case;
               when On_Long_Press =>
                  case B is
                     when Play =>
                        --  Switch to volume/BPM config mode
                        Current_Input_Mode := Volume_BPM;
                     when Rec =>
                        --  Switch to squence edition mode
                        Current_Input_Mode := Sequence_Edit;
                     when Track_A .. Track_E =>
                        --  Switch to channel assignment mode
                        Sequencer.Select_Track (To_Track (B));
                        Current_Input_Mode := Track_Assign;
                     when others => null;
                  end case;
               when On_Release =>
                  case B is
                     when Keyboard_Buttons =>
                        --  Release note or octave Up/Down
                        Sequencer.On_Release (B);
                     when others => null;
                  end case;
               when others => null;
            end case;

         when Sequence_Edit =>
            if B = Rec and then Evt = On_Release then
               Current_Input_Mode := Default_Input_Mode;
            end if;

         when Volume_BPM =>
            if B = Play and Evt = On_Release then
               Current_Input_Mode := Default_Input_Mode;
            end if;

         when FX_Select =>
            case Evt is
               when On_Press =>
                  case B is
                     when  B4 =>
                        Quick_Synth.Toggle_Solo (Track_A);
                     when  B5 =>
                        Quick_Synth.Toggle_Solo (Track_B);
                        FX_Is_On (B) := (Quick_Synth.In_Solo
                                         and then
                                         Quick_Synth.Solo = Track_B);
                     when  B6 =>
                        Quick_Synth.Toggle_Solo (Track_C);
                        FX_Is_On (B) := (Quick_Synth.In_Solo
                                         and then
                                         Quick_Synth.Solo = Track_C);
                     when  B7 =>
                        Quick_Synth.Toggle_Solo (Track_D);
                        FX_Is_On (B) := (Quick_Synth.In_Solo
                                         and then
                                         Quick_Synth.Solo = Track_D);
                     when  B8 =>
                        Quick_Synth.Toggle_Solo (Track_E);
                        FX_Is_On (B) := (Quick_Synth.In_Solo
                                         and then
                                         Quick_Synth.Solo = Track_E);
                     when  B1 .. B3 | B9 .. B16 =>
                        Set_FX (B);
                     when Track_A .. Track_E =>
                        Quick_Synth.Toggle_Mute (To_Track (B));
                     when others =>
                        null;
                  end case;
               when On_Release =>
                  if B = FX then
                     Current_Input_Mode := Default_Input_Mode;
                  end if;
               when others =>
                  null;
            end case;

         when Track_Assign =>
            if B in B1 .. B16 and then Evt = On_Press then
               Sequencer.Set_Instrument (To_Value (B));
            elsif B = To_Button (Sequencer.Track) and then Evt = On_Release then
               Current_Input_Mode := Default_Input_Mode;
            end if;
      end case;
   end Signal_Event;

   ------------
   -- Set_FX --
   ------------

   procedure Set_FX (B : Buttons) is
   begin
      FX_Is_On (B) := not FX_Is_On (B);
   end Set_FX;

   -----------
   -- Start --
   -----------

   procedure Start is
      Config : GPIO_Port_Configuration;
   begin

      Config.Speed := Speed_2MHz;

      -- Buttons --

      Config.Mode := Mode_In;
      Config.Output_Type := Push_Pull;
      Config.Resistors := Pull_Up;

      for Pt of Key_To_Point loop
         Enable_Clock (Pt);
         Pt.Configure_IO (Config);
      end loop;

      Enable_Clock (Wakeup);
      Config.Mode := Mode_Out;
      Config.Output_Type := Push_Pull;
      Config.Resistors := Floating;
      Wakeup.Configure_IO (Config);
      Wakeup.Clear;

      -- LEDs --

      Config.Mode := Mode_Out;
      Config.Output_Type := Push_Pull;
      Config.Resistors := Floating;

      for Pt of Row_To_Point loop
         Enable_Clock (Pt);
         Pt.Configure_IO (Config);
         Pt.Set;
      end loop;

      for Pt of Col_To_Point loop
         Enable_Clock (Pt);
         Pt.Configure_IO (Config);
         Pt.Clear;
      end loop;

      for B in LEDs loop
         Turn_Off (B);
      end loop;

      Set_True (UI_Task_Start);

      --  LED Timer --

      Enable_Clock (LED_Timer);

      Reset (LED_Timer);

      Configure (LED_Timer, Prescaler => 100, Period => 700);

      Enable_Interrupt (LED_Timer, Timer_Update_Interrupt);

      Enable (LED_Timer);
   end Start;

   ----------------
   -- Is_Pressed --
   ----------------

   function Is_Pressed (B : Buttons) return Boolean is
      (Key_State (B) = Down);

   -------------
   -- Turn_On --
   -------------

   procedure Turn_On (B : LEDs) is
   begin
      LED_State (B) := True;
   end Turn_On;

   --------------
   -- Turn_Off --
   --------------

   procedure Turn_Off (B : LEDs) is
   begin
      LED_State (B) := False;
   end Turn_Off;

   -------------
   -- Turn_On --
   -------------

   procedure Turn_On (LED_Addr : LED_Address)
   is
   begin
      Row_To_Point (LED_Addr.Row).Clear;
      Col_To_Point (LED_Addr.Col).Set;
   end Turn_On;

   --------------
   -- Turn_Off --
   --------------

   procedure Turn_Off (LED_Addr : LED_Address)
   is
   begin
      Row_To_Point (LED_Addr.Row).Set;
      Col_To_Point (LED_Addr.Col).Clear;
   end Turn_Off;

   -------------
   -- UI_Task --
   -------------

   task UI_Task is
      pragma Priority (UI_Task_Priority);
      pragma Storage_Size (512);
   end UI_Task;

   task body UI_Task is
      Period     : Time_Span renames UI_Task_Period;

      Next_Start : Time;
      Now        : Time renames Next_Start;
      Last_State    : array (Buttons) of Raw_Key_State := (others => Up);
      Pressed_Since : array (Buttons) of Time := (others => Time_First);
      Last_Event    : array (Buttons) of Buttton_Event := (others => On_Release);


      type Microstep_Cnt is mod 2;
      Microstep : Microstep_Cnt := 0;

      L_Enco : Integer;
      R_Enco : Integer;
   begin
      Suspend_Until_True (UI_Task_Start);

      Next_Start := Clock;
      loop
         Next_Start := Next_Start + Period;
         delay until Next_Start;

         Microstep := Microstep + 1;

         --  Handle buttons
         for B in Buttons loop
            Key_State (B) := (if Key_To_Point (B).Set then Up else Down);

            if Last_State (B) = Key_State (B) then
               --  The button didn't change, let's check if we are waiting for
               --  a long press event.
               if Has_Long_Press (B)
                 and then
                   Key_State (B) = Down
                 and then
                   Last_Event (B) = Waiting_For_Long_Press
                 and then
                   Pressed_Since (B) + Long_Press_Time_Span < Now
               then
                  Last_Event (B) := On_Long_Press;
                  Signal_Event (B, Last_Event (B));
               end if;

            elsif Key_State (B) = Down then
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

            Last_State (B) := Key_State (B);
         end loop;

         --------------
         -- Encoders --
         --------------

         if Microstep = 0 then
            L_Enco := WNM.Encoders.Left_Diff;
            R_Enco := WNM.Encoders.Right_Diff;

            case Current_Input_Mode is
            when Volume_BPM =>
               WNM.Sequencer.Set_BPM_Delta (R_Enco);
               WNM.Master_Volume.Change (L_Enco);
            when others =>
               null;
            end case;
         end if;

         --------------
         -- Set LEDs --
         --------------

         -- Tacks LEDs --
         for B in Buttons range Track_A .. Track_E loop

            if (Sequencer.Track = To_Track (B)
                and then Current_Input_Mode /= FX_Select)
              or else
                (not Quick_Synth.Muted (To_Track (B))
                 and then Current_Input_Mode = FX_Select)
            then
               Turn_On (B);
            else
               Turn_Off (B);
            end if;
         end loop;

         -- Play LED --
         if Sequencer.State in Play | Play_And_Rec then
            Turn_On (Play);
         else
            Turn_Off (Play);
         end if;

         -- Rec LED --
         if Sequencer.State = Rec
           or else
             (Sequencer.State = Play_And_Rec
              and then
              Sequencer.Step in 1 | 5 | 9 | 13)
         then
            Turn_On (Rec);
         else
            Turn_Off (Rec);
         end if;

         --  B1 .. B16 LEDs --
         case Current_Input_Mode is

            -- FX selection mode --
            when FX_Select =>
               Turn_Off (FX);
               --  The FX LED will be on if there's at least one FX enabled

               for B in B4 .. B8 loop
                  FX_Is_On (B) := (Quick_Synth.In_Solo
                                   and then
                                   Quick_Synth.Solo = (case B is
                                        when B4 => Track_A,
                                        when B5 => Track_B,
                                        when B6 => Track_C,
                                        when B7 => Track_D,
                                        when others => Track_E));
               end loop;

               for B in B1 .. B16 loop
                  if FX_Is_On (B) then
                     Turn_On (B);
                     Turn_On (FX);
                  else
                     Turn_Off (B);
                  end if;
               end loop;

            -- Track assign mode --
            when Track_Assign =>
               for B in B1 .. B16 loop
                  if Sequencer.Instrument (Sequencer.Track) = To_Value (B) then
                     Turn_On (B);
                  else
                     Turn_Off (B);
                  end if;
               end loop;

            --  Any other mode --
            when others =>

               for B in B1 .. B16 loop
                  Turn_Off (B);
               end loop;

               if Sequencer.State in Play_And_Rec | Play then
                  case Sequencer.Step is
                     when 1 => Turn_On (B1);
                     when 2 => Turn_On (B2);
                     when 3 => Turn_On (B3);
                     when 4 => Turn_On (B4);
                     when 5 => Turn_On (B5);
                     when 6 => Turn_On (B6);
                     when 7 => Turn_On (B7);
                     when 8 => Turn_On (B8);
                     when 9 => Turn_On (B9);
                     when 10 => Turn_On (B10);
                     when 11 => Turn_On (B11);
                     when 12 => Turn_On (B12);
                     when 13 => Turn_On (B13);
                     when 14 => Turn_On (B14);
                     when 15 => Turn_On (B15);
                     when 16 => Turn_On (B16);
                  end case;
               end if;
         end case;
      end loop;
   end UI_Task;


   -----------------------
   -- LED_Timer_Handler --
   -----------------------

   protected body LED_Timer_Handler is

      -----------------
      -- IRQ_Handler --
      -----------------

      procedure IRQ_Handler is
      begin
         if Status (LED_Timer, Timer_Update_Indicated) then
            if Interrupt_Enabled (LED_Timer, Timer_Update_Interrupt) then
               Clear_Pending_Interrupt (LED_Timer, Timer_Update_Interrupt);
            end if;

            Turn_Off (Key_To_LED (Current_LED));

            if Current_LED = LEDs'Last then
               Current_LED := LEDs'First;
            else
               Current_LED := LEDs'Succ (Current_LED);
            end if;

            if LED_State (Current_LED) then
               Turn_On (Key_To_LED (Current_LED));
            end if;
         end if;
      end IRQ_Handler;

   end LED_Timer_Handler;

end WNM.UI;
