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

with Ada.Real_Time; use Ada.Real_Time;
with System;
with MIDI;

package WNM is

   type Buttons is (B1, B2, B3, B4, B5, B6, B7, B8,
                    B9, B10, B11, B12, B13, B14, B15, B16,
                    Rec, Play, FX,
                    Track_A, Track_B, Track_C, Track_D, Track_E,
                    Encoder_L, Encoder_R);

   subtype Keyboard_Buttons is Buttons range B1 .. B16;

   type Keyboard_Value is range 1 .. 16;

   subtype LEDs is Buttons range B1 .. Track_E;

   function To_Value (B : Keyboard_Buttons) return Keyboard_Value;
   function To_Button (V : Keyboard_Value) return Keyboard_Buttons;

   subtype Tracks is Keyboard_Buttons;
   subtype Patterns is Keyboard_Buttons;

   type Trigger is (None, Always, Percent_25, Percent_50, Percent_75);

   function Image (Track : Tracks) return String;

   function To_Track (Chan : MIDI.MIDI_Channel) return Tracks
     with Inline_Always;
   function To_MIDI_Channel (Chan : Tracks) return MIDI.MIDI_Channel
     with Inline_Always;

   subtype Beat_Per_Minute is Positive range 50 .. 200;
   subtype Sequencer_Steps is Keyboard_Value;
   Steps_Per_Beat      : constant := 4;
   Max_Events_Per_Step : constant := 6;

   DAC_Task_Priority       : constant System.Priority := System.Default_Priority + 10;
   Synth_Task_Priority     : constant System.Priority := DAC_Task_Priority - 1;
   Sample_Task_Priority    : constant System.Priority := Synth_Task_Priority - 1;
   Sequencer_Task_Priority : constant System.Priority := Sample_Task_Priority - 1;
   UI_Task_Priority        : constant System.Priority := Sequencer_Task_Priority - 1;
   LED_Task_Priority       : constant System.Priority := UI_Task_Priority - 1;

   UI_Task_Period               : constant Time_Span := Milliseconds (50);
   UI_Task_Stack_Size           : constant := 10 * 1024;
   UI_Task_Secondary_Stack_Size : constant := 5 * 1024;

   Sequencer_Task_Stack_Size           : constant := 10 * 1024;
   Sequencer_Task_Secondary_Stack_Size : constant := 5 * 1024;

   Sample_Taks_Stack_Size       : constant := 10 * 1024;

   LED_Task_Period   : constant Time_Span := Microseconds (1000);

   Long_Press_Time_Span : constant Time_Span := Milliseconds (300);
   --  How much time users have to press a button to get the alternative
   --  function.

   Samples_Per_Buffer          : constant := 512;
   Mono_Buffer_Size_In_Bytes   : constant := Samples_Per_Buffer * 2;
   Stereo_Buffer_Size_In_Bytes : constant := Samples_Per_Buffer * 4;


   Sample_Rec_Filepath : constant String := "/sdcard/sample_rec.raw";
end WNM;
