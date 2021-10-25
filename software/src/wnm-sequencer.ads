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

with WNM.Time;
with WNM.Synth;
with WNM.MIDI;
with WNM.Sequence_Copy;

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

   procedure On_Press (Button : Keyboard_Button);

   procedure On_Release (Button : Keyboard_Button);

   procedure Select_Track (Track : Tracks);
   --  Select the current track

   procedure Do_Copy (T : in out WNM.Sequence_Copy.Copy_Transaction)
     with Pre => WNM.Sequence_Copy.Is_Complete (T);

   function Track return Tracks;

   procedure Set_Instrument (Val : Keyboard_Value);
   --  Set the instrument of the current track

   function Instrument (Track : Tracks) return Keyboard_Value;

   procedure Set_BPM (BPM : Beat_Per_Minute);
   procedure Change_BPM (BPM_Delta : Integer);
   function BPM return Beat_Per_Minute;
   function Samples_Per_Beat return Synth.Sample_Time;
   function Microseconds_Per_Beat return Time.Time_Microseconds;

   procedure Execute_Step;

   function Update return Time.Time_Microseconds;

   function Set (Step : Sequencer_Steps) return Boolean;
   function Set (Track : Tracks; Step : Sequencer_Steps) return Boolean;

   function Trig (Step : Sequencer_Steps) return WNM.Trigger;
   procedure Trig_Next (Step : Sequencer_Steps);
   procedure Trig_Prev (Step : Sequencer_Steps);

   function Repeat (Step : Sequencer_Steps) return WNM.Repeat;
   procedure Repeat_Next (Step : Sequencer_Steps);
   procedure Repeat_Prev (Step : Sequencer_Steps);

   function Repeat_Rate (Step : Sequencer_Steps) return WNM.Repeat_Rate;
   procedure Repeat_Rate_Next (Step : Sequencer_Steps);
   procedure Repeat_Rate_Prev (Step : Sequencer_Steps);

   function Note (Step : Sequencer_Steps) return MIDI.MIDI_Key;
   procedure Note_Next (Step : Sequencer_Steps);
   procedure Note_Prev (Step : Sequencer_Steps);

   function Velo (Step : Sequencer_Steps) return MIDI.MIDI_Data;
   procedure Velo_Next (Step : Sequencer_Steps);
   procedure Velo_Prev (Step : Sequencer_Steps);

   type CC_Id is (A, B, C, D);

   function CC_Enabled (Step : Sequencer_Steps; Id : CC_Id) return Boolean;
   procedure CC_Toggle (Step : Sequencer_Steps; Id : CC_Id);
   function CC_Value (Step : Sequencer_Steps; Id : CC_Id) return MIDI.MIDI_Data;
   procedure CC_Value_Inc (Step : Sequencer_Steps; Id : CC_Id);
   procedure CC_Value_Dec (Step : Sequencer_Steps; Id : CC_Id);

end WNM.Sequencer;
