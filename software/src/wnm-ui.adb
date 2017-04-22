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
--  with Test_I2S;
with WNM.Sequencer; use WNM.Sequencer;

package body WNM.UI is

   UI_Task_Start   : Suspension_Object;

   procedure Turn_On (LED_Addr : LED_Address);
   procedure Turn_Off (LED_Addr : LED_Address);
   procedure Signal_Event (B : Buttons; Evt : Buttton_Event);
   procedure Select_Channel (Chan : Channels);
   procedure Set_Volume (B : Buttons);

   procedure Set_FX (B : Buttons);
   procedure Set_Chan_Instrument (B : Buttons);

   type Input_Mode is (Note,
                       Sequence_Edit,
                       Volume_Select,
                       FX_Select,
                       Chan_Assign);

   Default_Input_Mode : constant Input_Mode := Note;

   Current_Volume : Buttons := B1;
   Current_Chan : Channels := Chan_A;
   Current_Instrument : array (Channels) of Buttons := (others => B1);
   FX_Is_On : array (Buttons) of Boolean := (others => False);
   Current_Input_Mode : Input_Mode := Note;

   ------------------
   -- Signal_Event --
   ------------------

--     Last_BPM_Press : Time := Time_First;
   procedure Signal_Event (B : Buttons; Evt : Buttton_Event) is
--        Now : constant Time := Clock;
   begin
      case Current_Input_Mode is
         when Note =>
            case Evt is
               when On_Press =>
                  case B is
--                       when  BPM_Vol =>
--                          --  Tap tempo
--                          if Now - Last_BPM_Press < Seconds (2) then
--                             Sequencer.Set_Beat_Period (Now - Last_BPM_Press);
--                          end if;
--                          Last_BPM_Press := Now;
                     when FX =>
                        --  Switch to FX mode
                        Current_Input_Mode := FX_Select;
                     when Play =>
                        Sequencer.Play_Pause;
                     when Rec =>
                        Sequencer.Rec;
                     when Chan_A .. Chan_E =>
                        --  Select current channel
                        Select_Channel (To_Channel (B));
                     when B1 =>
                        --  Octave Down...
                        null;
                     when B8 =>
                        --  Octave Up...
                        null;
                     when B9 .. B16 | B2 .. B3 | B5 .. B7 =>
                        --  Play note...
                        null;
--                          Test_I2S.Set_Current_Note (B);
                     when others => null;
                  end case;
               when On_Long_Press =>
                  case B is
--                       when BPM_Vol =>
--                          --  Switch to volume select mode
--                          Current_Input_Mode := Volume_Select;
                     when Rec =>
                        --  Switch to squence edition mode
                        Current_Input_Mode := Sequence_Edit;
                     when Chan_A .. Chan_E =>
                        --  Switch to channel assignment mode
                        Select_Channel (To_Channel (B));
                        Current_Input_Mode := Chan_Assign;
                     when others => null;
                  end case;
               when others => null;
            end case;

         when Sequence_Edit =>
            if B = Rec and then Evt = On_Release then
               Current_Input_Mode := Default_Input_Mode;
            end if;

         when Volume_Select =>
            if B in B1 .. B16 and then Evt = On_Press then
               Set_Volume (B);
--              elsif B = BPM_Vol and then Evt = On_Release then
--                 Current_Input_Mode := Default_Input_Mode;
            end if;

         when FX_Select =>
            if B in B1 .. B16 and then Evt = On_Press then
               Set_FX (B);
            elsif B = FX and then Evt = On_Release then
               Current_Input_Mode := Default_Input_Mode;
            end if;

         when Chan_Assign =>
            if B in B1 .. B16 and then Evt = On_Press then
               Set_Chan_Instrument (B);
            elsif B = To_Button (Current_Chan) and then Evt = On_Release then
               Current_Input_Mode := Default_Input_Mode;
            end if;
      end case;
   end Signal_Event;

   --------------------
   -- Select_Channel --
   --------------------

   procedure Select_Channel (Chan : Channels) is
   begin
      Turn_Off (Chan_A);
      Turn_Off (Chan_B);
      Turn_Off (Chan_C);
      Turn_Off (Chan_D);
      Turn_Off (Chan_E);
      Turn_On (To_Button (Chan));
--        Test_I2S.Set_Current_Channel (Chan);
      Current_Chan := Chan;
   end Select_Channel;

   ----------------
   -- Set_Volume --
   ----------------

   procedure Set_Volume (B : Buttons) is
   begin
      Current_Volume := B;
   end Set_Volume;

   ------------
   -- Set_FX --
   ------------

   procedure Set_FX (B : Buttons) is
   begin
--        if B = B1 then
--           if FX_Is_On (B) then
--              Test_I2S.Disable_FX;
--           else
--              Test_I2S.Enable_FX;
--           end if;
--        end if;
      FX_Is_On (B) := not FX_Is_On (B);
   end Set_FX;

   -------------------------
   -- Set_Chan_Instrument --
   -------------------------

   procedure Set_Chan_Instrument (B : Buttons) is
   begin
      Current_Instrument (Current_Chan) := B;
   end Set_Chan_Instrument;

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

      Configure (LED_Timer, Prescaler => 100, Period => 800);

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
   begin
      Suspend_Until_True (UI_Task_Start);

      Next_Start := Clock;
      loop
         Next_Start := Next_Start + Period;
         delay until Next_Start;

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

         ---------------
         --  Set LEDs --
         ---------------

         for B in Buttons range Chan_A .. Chan_E loop
            if Current_Chan = To_Channel (B) then
               Turn_On (B);
            else
               Turn_Off (B);
            end if;
         end loop;

         if Sequencer.State = Play or else Sequencer.State = Play_And_Rec then
            Turn_On (Play);
         else
            Turn_Off (Play);
         end if;

         case Sequencer.Step is
            when 1 | 5 | 9 | 13 =>
--                 Turn_On (BPM_Vol);
               if Sequencer.State = Rec or else Sequencer.State = Play_And_Rec then
                  Turn_On (Rec);
               end if;
            when others =>
--                 Turn_Off (BPM_Vol);
               if Sequencer.State = Rec then
                  Turn_Off (Rec);
               end if;
         end case;

         case Current_Input_Mode is
            when Volume_Select =>
               for B in B1 .. B16 loop
                  if B <= Current_Volume then
                     Turn_On (B);
                  else
                     Turn_Off (B);
                  end if;
               end loop;
            when FX_Select =>
               Turn_Off (FX);
               --  The FX LED will be on if there's at least one FX enabled
               for B in B1 .. B16 loop
                  if FX_Is_On (B) then
                     Turn_On (B);
                     Turn_On (FX);
                  else
                     Turn_Off (B);
                  end if;
               end loop;
            when Chan_Assign =>
               for B in B1 .. B16 loop
                  if Current_Instrument (Current_Chan) = B then
                     Turn_On (B);
                  else
                     Turn_Off (B);
                  end if;
               end loop;
            when Note =>
               for B in B1 .. B16 loop
                  Turn_Off (B);
               end loop;
            when Sequence_Edit =>
               null;
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
