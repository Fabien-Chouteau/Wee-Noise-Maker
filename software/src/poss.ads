-------------------------------------------------------------------------------
--                                                                           --
--                       Pocket Open Source Synthesizer                      --
--                                                                           --
--                     Copyright (C) 2016 Fabien Chouteau                    --
--                                                                           --
--    POSS is free software: you can redistribute it and/or modify it        --
--    under the terms of the GNU General Public License as published by      --
--    the Free Software Foundation, either version 3 of the License, or      --
--    (at your option) any later version.                                    --
--                                                                           --
--    POSS is distributed in the hope that it will be useful, but WITHOUT    --
--    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY     --
--    or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public        --
--    License for more details.                                              --
--                                                                           --
--    You should have received a copy of the GNU General Public License      --
--    along with POSS. If not, see <http://www.gnu.org/licenses/>.           --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Real_Time; use Ada.Real_Time;
with System;

package POSS is

   type Buttons is (B1, B2, B3, B4, B5, B6, B7, B8,
                    B9, B10, B11, B12, B13, B14, B15, B16,
                    Rec, Play, FX, BPM_Vol,
                    Chan_A, Chan_B, Chan_C, Chan_D, Chan_E);

   subtype Channel_Buttons is Buttons range Chan_A .. Chan_E;
   subtype Keyboard_Buttons is Buttons range B1 .. B16;

   type Keyboard_Value is range 1 .. 16;

   subtype LEDs is Buttons;

   function To_Value (B : Keyboard_Buttons) return Keyboard_Value;

   type Channels is (Chan_A, Chan_B, Chan_C, Chan_D, Chan_E);

   function To_Channel (B : Channel_Buttons) return Channels;
   function To_Button (Chan : Channels) return Channel_Buttons;

   type Sequencer_Steps is range 1 .. 16;
   Steps_Per_Beat      : constant := 4;
   Max_Events_Per_Step : constant := 4;

   UI_Task_Period   : constant Time_Span := Milliseconds (10);
   UI_Task_Priority : constant System.Priority := System.Default_Priority;

   LED_Task_Period   : constant Time_Span := Microseconds (1000);
   LED_Task_Priority : constant System.Priority := System.Default_Priority;

   Sequencer_Task_Priority : constant System.Priority := System.Default_Priority;

   Synth_Task_Priority : constant System.Priority := System.Default_Priority + 1;
   Long_Press_Time_Span : constant Time_Span := Milliseconds (300);
   --  How much 2time users have to press a button to get the alternative
   --  function.

end POSS;
