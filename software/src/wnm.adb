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

   --------------
   -- To_Track --
   --------------

   function To_Track (B : Tacks_Buttons) return Tracks
   is (case B is
          when Track_A => Track_A,
          when Track_B => Track_B,
          when Track_C => Track_C,
          when Track_D => Track_D,
          when Track_E => Track_E);

   ---------------
   -- To_Button --
   ---------------

   function To_Button (Chan : Tracks) return Tacks_Buttons
   is (case Chan is
          when Track_A => Track_A,
          when Track_B => Track_B,
          when Track_C => Track_C,
          when Track_D => Track_D,
          when Track_E => Track_E);

   --------------
   -- To_Track --
   --------------

   function To_Track (Chan : MIDI.MIDI_Channel) return Tracks
   is (case Chan is
          when 0      => Track_A,
          when 1      => Track_B,
          when 2      => Track_C,
          when 3      => Track_D,
          when others => Track_E);

   ---------------------
   -- To_MIDI_Channel --
   ---------------------

   function To_MIDI_Channel (Chan : Tracks) return MIDI.MIDI_Channel
   is (case Chan is
          when Track_A => 0,
          when Track_B => 1,
          when Track_C => 2,
          when Track_D => 3,
          when Track_E => 4);
end WNM;
