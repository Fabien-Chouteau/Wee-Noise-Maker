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

private with WNM.Sequence;

package WNM.Sequencer is

   type Sequencer_State is (Pause, Play, Rec, Play_And_Rec);

   function State return Sequencer_State;
   --  Current state of the sequencer

   function Step return Sequencer_Steps;
   --  Current step

   procedure Play_Pause;
   --  Use it to signal a play/pause event

   procedure Rec;
   --  Use it to signal a rec/stop record event

   procedure On_Press (Button : Keyboard_Buttons);

   procedure On_Release (Button : Keyboard_Buttons);

   procedure Select_Channel (Chan : Channels);
   --  Select the current channel

   procedure Set_Instrument (Val : Keyboard_Value);
   --  Set the instrument of the current channel

   procedure Set_BPM (BPM : Positive);
   --  Also known as 60/BPM...

   procedure Start;
   --  Start the sequencer task

private

   Sequences : array (Channels) of WNM.Sequence.Instance;

end WNM.Sequencer;
