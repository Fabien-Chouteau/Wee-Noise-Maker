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

with HAL.Audio;
with MIDI;
with WNM;

package Quick_Synth is

   procedure Event (Msg : MIDI.Message);
   procedure Fill (Input  :     HAL.Audio.Audio_Buffer;
                   Output : out HAL.Audio.Audio_Buffer);

   procedure Mute (Track : WNM.Tracks);
   procedure Unmute (Track : WNM.Tracks);
   procedure Toggle_Mute (Track : WNM.Tracks);
   function Muted (Track : WNM.Tracks) return Boolean;

   procedure Toggle_Solo (Track : WNM.Tracks);
   function In_Solo return Boolean;
   function Solo return WNM.Tracks;

   procedure Change_Pan (Track : WNM.Tracks;
                         Pan   : Integer);
   function Pan (Track : WNM.Tracks) return Integer;

end Quick_Synth;
