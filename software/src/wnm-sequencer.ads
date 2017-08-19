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

package WNM.Sequencer is

   type Sequencer_State is (Pause,
                            Play,
                            Edit,
                            Play_And_Rec,
                            Play_And_Edit);

   function State return Sequencer_State;
   --  Current state of the sequencer

   function Step return Sequencer_Steps;
   --  Current step

   procedure Play_Pause;
   --  Use it to signal a play/pause event

   procedure Rec_Pressed;
   --  Use it to signal a rec/stop record event

   procedure Rec_Long;

   procedure Rec_Release;

   procedure On_Press (Button : Keyboard_Buttons);

   procedure On_Release (Button : Keyboard_Buttons);

   procedure Select_Track (Track : Tracks);
   --  Select the current track

   function Track return Tracks;

   procedure Set_Instrument (Val : Keyboard_Value);
   --  Set the instrument of the current track

   function Instrument (Track : Tracks) return Keyboard_Value;

   procedure Set_BPM (BPM : Beat_Per_Minute);
   procedure Change_BPM (BPM_Delta : Integer);
   function BPM return Beat_Per_Minute;

   procedure Execute_Step;

   procedure Start;
   --  Start the sequencer task

   function Set (Step : Sequencer_Steps) return Boolean;
   function Set (Track : Tracks; Step : Sequencer_Steps) return Boolean;

   function Trig (Step : Sequencer_Steps) return Trigger;
   procedure Trig_Next (Step : Sequencer_Steps);
   procedure Trig_Prev (Step : Sequencer_Steps);

end WNM.Sequencer;
