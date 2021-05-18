-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2021 Fabien Chouteau                    --
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

with WNM.Synth;
with WNM.MIDI;

package WNM.Short_Term_Sequencer is

   subtype Data is WNM.MIDI.Message;

   procedure Push (D : Data; Expiration : Synth.Sample_Time);
   procedure Pop (Now : Synth.Sample_Time; D : out Data; Success : out Boolean);

private

   MAX_EVENT_NUMBER : constant := 256;

   type Event;

   type Event_Access is access all Event;

   type Event is record
      D : Data;
      Expiration : Synth.Sample_Time;
      Next : Event_Access := null;
   end record;

end WNM.Short_Term_Sequencer;
