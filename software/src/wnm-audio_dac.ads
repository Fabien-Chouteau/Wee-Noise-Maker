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

with WNM.I2C;
with HAL.Audio;

package WNM.Audio_DAC is

   function Initialized return Boolean;

   procedure Initialize
     with Pre  => WNM.I2C.Initialized,
          Post => Initialized;

   type Audio_Buffer_Access is access all HAL.Audio.Audio_Buffer;

   procedure Give_Buffer (Buf : not null Audio_Buffer_Access);

   type DAC_Volume is range 0 .. 100;
   procedure Set_Volume (Volume : DAC_Volume);

end WNM.Audio_DAC;