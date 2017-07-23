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

with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;
with MIDI;
with Quick_Synth;
with WNM.Sequence;                 use WNM.Sequence;

package body WNM.Sequencer is

   use type MIDI.Octaves;

   Start_Sequencer_Task : Suspension_Object;

   Sequencer_BPM : Beat_Per_Minute := Beat_Per_Minute'First;

   Current_Step      : Sequencer_Steps := Sequencer_Steps'First with Atomic;
   Current_Seq_State : Sequencer_State := Pause with Atomic;
   Current_Track     : Tracks := Tracks'First;
   Track_Instrument  : array (Tracks) of Keyboard_Value :=
     (others => Keyboard_Value'First);

   type Sequencer_State_Event is (Play_Event, Rec_Event);

   function Transition (State : Sequencer_State;
                        Evt   : Sequencer_State_Event)
                        return Sequencer_State
   is
     (case State is
         when Pause =>        (case Evt is
                                  when Play_Event => Play,
                                  when Rec_Event  => Rec),
         when Play =>         (case Evt is
                                  when Play_Event  => Pause,
                                  when Rec_Event   => Play_And_Rec),
         when Rec =>          (case Evt is
                                  when Play_Event => Play_And_Rec,
                                  when Rec_Event  => Pause),
         when Play_And_Rec => (case Evt is
                                  when Play_Event => Rec,
                                  when Rec_Event  => Play));

   Octave : MIDI.Octaves := 4;

   function To_Key (B : Keyboard_Buttons; Oct : MIDI.Octaves) return MIDI.MIDI_Key is
     (case B is
         when B9  => MIDI.Key (Oct, MIDI.C),
         when B2  => MIDI.Key (Oct, MIDI.Cs),
         when B10 => MIDI.Key (Oct, MIDI.D),
         when B3  => MIDI.Key (Oct, MIDI.Ds),
         when B11 => MIDI.Key (Oct, MIDI.E),
         when B12 => MIDI.Key (Oct, MIDI.F),
         when B5  => MIDI.Key (Oct, MIDI.Fs),
         when B13 => MIDI.Key (Oct, MIDI.G),
         when B6  => MIDI.Key (Oct, MIDI.Gs),
         when B14 => MIDI.Key (Oct, MIDI.A),
         when B7  => MIDI.Key (Oct, MIDI.As),
         when B15 => MIDI.Key (Oct, MIDI.B),
         when B16 => MIDI.Key (Oct + 1, MIDI.C),
         when others => MIDI.A0);

   -----------
   -- State --
   -----------

   function State return Sequencer_State is
     (Current_Seq_State);

   ----------
   -- Step --
   ----------

   function Step return Sequencer_Steps is
      (Current_Step);

   ----------------
   -- Play_Pause --
   ----------------

   procedure Play_Pause is
   begin
      Current_Seq_State := Transition (Current_Seq_State, Play_Event);
      if Current_Seq_State in Play | Play_And_Rec then
         Current_Step := Sequencer_Steps'First;
      end if;
   end Play_Pause;

   ---------
   -- Rec --
   ---------

   procedure Rec is
   begin
      Current_Seq_State := Transition (Current_Seq_State, Rec_Event);
   end Rec;

   --------------
   -- On_Press --
   --------------

   procedure On_Press (Button : Keyboard_Buttons) is
   begin
      case Button is
         when B1 =>
            if Octave > MIDI.Octaves'First then
               Octave := Octave - 1;
            end if;
         when B8 =>
            if Octave < MIDI.Octaves'Last - 1 then
               Octave := Octave + 1;
            end if;
         when B4 =>
            null;
         when B2 .. B3 | B5 .. B7 | B9 .. B16 =>
            declare
               Msg : MIDI.Message (MIDI.Note_On);
            begin
               Msg.Channel := To_MIDI_Channel (Current_Track);
               Msg.Cmd.Key := To_Key (Button, Octave);
               Msg.Cmd.Velocity := 16#77#;
               Quick_Synth.Event (Msg);

               if Current_Seq_State in Play_And_Rec | Rec then
                  Add (Sequences (Current_Track), Current_Step, Msg.Cmd);
               end if;
            end;
      end case;
   end On_Press;

   ----------------
   -- On_Release --
   ----------------

   procedure On_Release (Button : Keyboard_Buttons) is
   begin
      case Button is
         when B2 .. B3 | B5 .. B7 | B9 .. B16 =>
            declare
               Msg : MIDI.Message (MIDI.Note_Off);
            begin
               Msg.Channel := To_MIDI_Channel (Current_Track);
               Msg.Cmd.Key := To_Key (Button, Octave);
               Msg.Cmd.Velocity := 16#77#;
               Quick_Synth.Event (Msg);

               if Current_Seq_State in Play_And_Rec | Rec then
                  Add (Sequences (Current_Track), Current_Step, Msg.Cmd);
               end if;
            end;
         when others =>
            null;
      end case;
   end On_Release;

   --------------------
   -- Select_Channel --
   --------------------

   procedure Select_Track (Track : Tracks) is
   begin
      Current_Track := Track;
   end Select_Track;

   -----------
   -- Track --
   -----------

   function Track return Tracks
   is (Current_Track);

   --------------------
   -- Set_Instrument --
   --------------------

   procedure Set_Instrument (Val : Keyboard_Value) is
   begin
      Track_Instrument (Current_Track) := Val;
   end Set_Instrument;

   ----------------
   -- Instrument --
   ----------------

   function Instrument (Track : Tracks) return Keyboard_Value
   is (Track_Instrument (Track));

   -------------
   -- Set_BPM --
   -------------

   procedure Set_BPM (BPM : Beat_Per_Minute) is
   begin
      Sequencer_BPM := BPM;
   end Set_BPM;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      Set_True (Start_Sequencer_Task);
   end Start;

   --------------------
   -- Sequencer_Task --
   --------------------

   task Sequencer_Task is
      pragma Priority (Sequencer_Task_Priority);
   end Sequencer_Task;

   task body Sequencer_Task is
      Next_Start : Time;

      type Microstep_Cnt is mod 2;
      Microstep : Microstep_Cnt := 1;
   begin
      Suspend_Until_True (Start_Sequencer_Task);
      Next_Start := Clock;
      loop
         Next_Start := Next_Start +
           (Milliseconds ((60 * 1000) / Sequencer_BPM) / Steps_Per_Beat) / 2;
         delay until Next_Start;

         case Microstep is

         --  Begining of a new step
         when 0 =>
            if Current_Step /= Sequencer_Steps'Last then
               Current_Step := Current_Step + 1;
            else
               Current_Step := Sequencer_Steps'First;
            end if;

         --  At the middle of the step we play the recorded notes
         when 1 =>
            if Current_Seq_State in Play | Play_And_Rec then
               for Track in Sequences'Range loop
                  for Index in 1 .. Last_Index (Sequences (Track), Current_Step) loop
                     declare
                        Midi_Cmd : constant MIDI.Command := Cmd (Sequences (Track),
                                                              Current_Step,
                                                                 Index);
                        Msg : MIDI.Message (Midi_Cmd.Kind);
                     begin
                        Msg.Cmd := Midi_Cmd;
                        Msg.Channel := To_MIDI_Channel (Track);

                        Quick_Synth.Event (Msg);
                     end;
                  end loop;
               end loop;
            end if;
         end case;
         Microstep := Microstep + 1;
      end loop;

   end Sequencer_Task;

end WNM.Sequencer;
