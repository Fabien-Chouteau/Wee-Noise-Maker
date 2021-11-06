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
with WNM.UI;

package WNM.Sequencer is

   --  type Sequencer_State is (Pause,
   --                           Play,
   --                           Edit,
   --                           Play_And_Rec,
   --                           Play_And_Edit);
   --
   --  function State return Sequencer_State;
   --  --  Current state of the sequencer

   function Playing_Step return Sequencer_Steps;
   function Playing_Pattern return Patterns;

   function Editing_Step return Sequencer_Steps;
   function Editing_Track return Tracks;
   function Editing_Pattern return Patterns;

   procedure Set_Editing_Step    (S : Sequencer_Steps);
   procedure Set_Editing_Track   (T : Tracks);
   procedure Set_Editing_Pattern (P : Patterns);

   procedure Play_Pause;
   --  Use it to signal a play/pause event

   procedure On_Press (Button : Keyboard_Button;
                       Mode : WNM.UI.Main_Modes);

   procedure On_Release (Button : Keyboard_Button;
                         Mode : WNM.UI.Main_Modes);

   procedure Do_Copy (T : in out WNM.Sequence_Copy.Copy_Transaction)
     with Pre => WNM.Sequence_Copy.Is_Complete (T);

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

   type Note_Mode_Kind is (Note, Chord, Note_In_Chord);
   package Note_Mode_Kind_Next is new Enum_Next (Note_Mode_Kind);
   use Note_Mode_Kind_Next;

   function Img (M : Note_Mode_Kind) return String
   is (case M is
          when Note => "Note",
          when Chord => "Chord",
          when Note_In_Chord => "Note in chord");

   function Note_Mode (Step : Sequencer_Steps) return Note_Mode_Kind;
   procedure Note_Mode_Next (Step : Sequencer_Steps);

   function Note (Step : Sequencer_Steps) return MIDI.MIDI_Key;
   procedure Note_Next (Step : Sequencer_Steps);
   procedure Note_Prev (Step : Sequencer_Steps);

   function Duration (Step : Sequencer_Steps) return Note_Duration;
   procedure Duration_Next (Step : Sequencer_Steps);
   procedure Duration_Prev (Step : Sequencer_Steps);

   function Velo (Step : Sequencer_Steps) return MIDI.MIDI_Data;
   procedure Velo_Next (Step : Sequencer_Steps);
   procedure Velo_Prev (Step : Sequencer_Steps);

   function MIDI_Chan (T : Tracks) return MIDI.MIDI_Channel;
   procedure MIDI_Chan_Next (T : Tracks);
   procedure MIDI_Chan_Prev (T : Tracks);

   type CC_Id is (A, B, C, D);

   function CC_Letter (ID : Sequencer.CC_Id) return String
   is (case ID is
          when A => "A",
          when B => "B",
          when C => "C",
          when D => "D");

   function CC_Controller (T : Tracks; Id : CC_Id) return MIDI.MIDI_Data;
   procedure Set_CC_Controller (T : Tracks; Id : CC_Id; C : MIDI.MIDI_Data);
   procedure CC_Controller_Next (T : Tracks; Id : CC_Id);
   procedure CC_Controller_Prev (T : Tracks; Id : CC_Id);

   subtype Controller_Label is String (1 .. 17);
   Empty_Controller_Label : constant Controller_Label := (others => ' ');

   procedure Set_CC_Controller_Label (T    : Tracks;
                                      Id   : CC_Id;
                                      Label : Controller_Label);
   function CC_Controller_Label (T    : Tracks;
                                 Id   : CC_Id)
                                 return Controller_Label;

   function CC_Enabled (Step : Sequencer_Steps; Id : CC_Id) return Boolean;
   procedure CC_Toggle (Step : Sequencer_Steps; Id : CC_Id);
   function CC_Value (Step : Sequencer_Steps; Id : CC_Id) return MIDI.MIDI_Data;
   procedure CC_Value_Inc (Step : Sequencer_Steps; Id : CC_Id);
   procedure CC_Value_Dec (Step : Sequencer_Steps; Id : CC_Id);

end WNM.Sequencer;
