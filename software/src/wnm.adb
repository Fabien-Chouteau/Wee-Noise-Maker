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

   ----------------
   -- To_Channel --
   ----------------

   function To_Channel (B : Channel_Buttons) return Channels is
     (case B is
         when Chan_A => Chan_A,
         when Chan_B => Chan_B,
         when Chan_C => Chan_C,
         when Chan_D => Chan_D,
         when Chan_E => Chan_E);

   ---------------
   -- To_Button --
   ---------------

   function To_Button (Chan : Channels) return Channel_Buttons is
     (case Chan is
         when Chan_A => Chan_A,
         when Chan_B => Chan_B,
         when Chan_C => Chan_C,
         when Chan_D => Chan_D,
         when Chan_E => Chan_E);

end WNM;
