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

package body WNM is

   --------------
   -- To_Value --
   --------------

   function To_Value (B : Keyboard_Buttons) return Keyboard_Value is
   begin
      return Keyboard_Value (B'Enum_Rep - B1'Enum_Rep + 1);
   end To_Value;

   ---------------
   -- To_Button --
   ---------------

   function To_Button (V : Keyboard_Value) return Keyboard_Buttons
   is (case V is
          when 1  => B1,
          when 2  => B2,
          when 3  => B3,
          when 4  => B4,
          when 5  => B5,
          when 6  => B6,
          when 7  => B7,
          when 8  => B8,
          when 9  => B9,
          when 10 => B10,
          when 11 => B11,
          when 12 => B12,
          when 13 => B13,
          when 14 => B14,
          when 15 => B15,
          when 16 => B16);

   -----------
   -- Image --
   -----------

   function Image (Track : Tracks) return String
   is ("Track " & (case Track is
                      when B1  => "1",
                      when B2  => "2",
                      when B3  => "3",
                      when B4  => "4",
                      when B5  => "5",
                      when B6  => "6",
                      when B7  => "7",
                      when B8  => "8",
                      when B9  => "9",
                      when B10 => "10",
                      when B11 => "11",
                      when B12 => "12",
                      when B13 => "13",
                      when B14 => "14",
                      when B15 => "15",
                      when B16 => "16"));

   --------------
   -- To_Track --
   --------------

   function To_Track (Chan : MIDI.MIDI_Channel) return Tracks
   is (case Chan is
          when 0      => B1,
          when 1      => B2,
          when 2      => B3,
          when 3      => B4,
          when 4      => B5,
          when 5      => B6,
          when 6      => B7,
          when 7      => B8,
          when 8      => B9,
          when 9      => B10,
          when 10     => B11,
          when 11     => B12,
          when 12     => B13,
          when 13     => B14,
          when 14     => B15,
          when 15     => B16);

   ---------------------
   -- To_MIDI_Channel --
   ---------------------

   function To_MIDI_Channel (Chan : Tracks) return MIDI.MIDI_Channel
   is (case Chan is
          when B1  => 0,
          when B2  => 1,
          when B3  => 2,
          when B4  => 3,
          when B5  => 4,
          when B6  => 5,
          when B7  => 6,
          when B8  => 7,
          when B9  => 8,
          when B10 => 9,
          when B11 => 10,
          when B12 => 11,
          when B13 => 12,
          when B14 => 13,
          when B15 => 14,
          when B16 => 15);
end WNM;
