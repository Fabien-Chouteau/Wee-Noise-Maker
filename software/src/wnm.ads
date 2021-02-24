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

with MIDI;
with Enum_Next;

package WNM is

   type Button is (B1, B2, B3, B4, B5, B6, B7, B8,
                   B9, B10, B11, B12, B13, B14, B15, B16,
                   Rec, Play,
                   Menu, Func, Track_Button, Pattern,
                   Encoder_L, Encoder_R);

   subtype Keyboard_Button is Button range B1 .. B16;

   type Keyboard_Value is range 1 .. 16;

   subtype LEDs is Button range B1 .. Play;

   function To_Value (B : Keyboard_Button) return Keyboard_Value;
   function To_Button (V : Keyboard_Value) return Keyboard_Button;

   subtype Tracks is Keyboard_Button;
   subtype Patterns is Keyboard_Button;

   type Trigger is (None, Always, Fill, Percent_25, Percent_50, Percent_75);
   type Repeat is mod 9;
   type Repeat_Rate is (Rate_1_1, Rate_1_2, Rate_1_3, Rate_1_4, Rate_1_5,
                        Rate_1_6, Rate_1_8, Rate_1_10, Rate_1_12, Rate_1_16,
                        Rate_1_20, Rate_1_24, Rate_1_32);

   function To_Track (Chan : MIDI.MIDI_Channel) return Tracks
     with Inline_Always;
   function To_MIDI_Channel (Chan : Tracks) return MIDI.MIDI_Channel
     with Inline_Always;

   subtype Beat_Per_Minute is Positive range 50 .. 200;
   subtype Sequencer_Steps is Keyboard_Value;
   Steps_Per_Beat      : constant := 4;
   Max_Events_Per_Step : constant := 6;

   UI_Task_Period_Ms : constant := 50;
   GUI_Task_Period_Ms : constant := 50;
   LED_Task_Period_Ms : constant := 50;

   Long_Press_Time_Span_Ms : constant := 300;
   --  How much time (in miliseconds) users have to press a button to get the
   --  alternative function.

   Sample_Frequency            : constant := 44_100;
   Samples_Per_Buffer          : constant := 512;
   Mono_Buffer_Size_In_Bytes   : constant := Samples_Per_Buffer * 2;
   Stereo_Buffer_Size_In_Bytes : constant := Samples_Per_Buffer * 4;

   Audio_Queue_Size : constant := 3;

   Sample_Rec_Filepath : constant String := "/sample_rec.raw";

   --  Enums utils --
   pragma Warnings (Off, "use clause for package");
   package Trigger_Next is new Enum_Next (Trigger);
   use Trigger_Next;
   package Repeat_Next is new Enum_Next (Repeat);
   use Repeat_Next;
   package Repeat_Rate_Next is new Enum_Next (Repeat_Rate);
   use Repeat_Rate_Next;
   pragma Warnings (On, "use clause for package");

end WNM;
