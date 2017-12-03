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

with Ada.Synchronous_Task_Control;
with MIDI;
with Quick_Synth;
with WNM.Sequence;          use WNM.Sequence;
with WNM.Pattern_Sequencer;
with HAL;                   use HAL;

package body WNM.Sequencer is

   use type MIDI.Octaves;

   type Pattern is array (Tracks) of WNM.Sequence.Instance;
   Sequences : array (Patterns) of Pattern;

   Sequencer_BPM : Beat_Per_Minute := 90;

   Current_Step      : Sequencer_Steps := Sequencer_Steps'First with Atomic;
   Current_Seq_State : Sequencer_State := Pause with Atomic;
   Current_Track     : Tracks := Tracks'First with Atomic;
   Track_Instrument  : array (Tracks) of Keyboard_Value :=
     (others => Keyboard_Value'First);

   Task_Start     : Ada.Synchronous_Task_Control.Suspension_Object;
   Next_Start     : Time := Time_Last;

   task Sequencer_Task
     with Priority             => Sequencer_Task_Priority,
          Storage_Size         => Sequencer_Task_Stack_Size,
          Secondary_Stack_Size => Sequencer_Task_Secondary_Stack_Size;

   type Microstep_Cnt is mod 2;
   Microstep : Microstep_Cnt := 1;

   procedure Process_Step (Pattern : Patterns; Step : Sequencer_Steps);

   Rand_X : UInt32 := 123456789;
   Rand_Y : UInt32 := 362436069;
   Rand_Z : UInt32 := 521288629;

   type Rand_Percent is range 0 .. 100;
   function Random return Rand_Percent;

   type Sequencer_State_Event is (Play_Event,
                                  Rec_Event,
                                  Rec_Long_Event,
                                  Rec_Release_Event);

   function Transition (State : Sequencer_State;
                        Evt   : Sequencer_State_Event)
                        return Sequencer_State
   is
     (case State is
         when Pause =>        (case Evt is
                                  when Play_Event        => Play,
                                  when Rec_Event         => Edit,
                                  when Rec_Long_Event    => Pause,
                                  when Rec_Release_Event => Pause),
         when Play =>         (case Evt is
                                  when Play_Event        => Pause,
                                  when Rec_Event         => Play_And_Edit,
                                  when Rec_Long_Event    => Play_And_Rec,
                                  when Rec_Release_Event => Play),
         when Edit =>         (case Evt is
                                  when Play_Event        => Play_And_Edit,
                                  when Rec_Event         => Pause,
                                  when Rec_Long_Event    => Edit,
                                  when Rec_Release_Event => Edit),
         when Play_And_Edit =>         (case Evt is
                                  when Play_Event        => Edit,
                                  when Rec_Event         => Play,
                                  when Rec_Long_Event    => Play_And_Rec,
                                  when Rec_Release_Event => Play_And_Edit),
         when Play_And_Rec => (case Evt is
                                  when Play_Event        => Pause,
                                  when Rec_Event         => Play_And_Rec,
                                  when Rec_Long_Event    => Play_And_Rec,
                                  when Rec_Release_Event => Play));

--      Octave : MIDI.Octaves := 4;

--     function To_Key (B : Keyboard_Buttons; Oct : MIDI.Octaves) return MIDI.MIDI_Key is
--       (case B is
--           when B9  => MIDI.Key (Oct, MIDI.C),
--           when B2  => MIDI.Key (Oct, MIDI.Cs),
--           when B10 => MIDI.Key (Oct, MIDI.D),
--           when B3  => MIDI.Key (Oct, MIDI.Ds),
--           when B11 => MIDI.Key (Oct, MIDI.E),
--           when B12 => MIDI.Key (Oct, MIDI.F),
--           when B5  => MIDI.Key (Oct, MIDI.Fs),
--           when B13 => MIDI.Key (Oct, MIDI.G),
--           when B6  => MIDI.Key (Oct, MIDI.Gs),
--           when B14 => MIDI.Key (Oct, MIDI.A),
--           when B7  => MIDI.Key (Oct, MIDI.As),
--           when B15 => MIDI.Key (Oct, MIDI.B),
--           when B16 => MIDI.Key (Oct + 1, MIDI.C),
--           when others => MIDI.A0);

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
         Microstep := 0;
      end if;
   end Play_Pause;

   -----------------
   -- Rec_Pressed --
   -----------------

   procedure Rec_Pressed is
   begin
      Current_Seq_State := Transition (Current_Seq_State, Rec_Event);
   end Rec_Pressed;

   --------------
   -- Rec_Long --
   --------------

   procedure Rec_Long is
   begin
      Current_Seq_State := Transition (Current_Seq_State, Rec_Long_Event);
   end Rec_Long;

   -----------------
   -- Rec_Release --
   -----------------

   procedure Rec_Release is
   begin
      Current_Seq_State := Transition (Current_Seq_State, Rec_Release_Event);
   end Rec_Release;

   --------------
   -- On_Press --
   --------------

   procedure On_Press (Button : Keyboard_Button) is
   begin
      case Current_Seq_State is
         when Pause | Play =>
            Quick_Synth.Trig (Button);
            Current_Track := Button;
         when Play_And_Rec =>
            Set (Sequences (Pattern_Sequencer.Current_Pattern)(Button),
                 Current_Step);
            Current_Track := Button;

            if Microstep /= 1 then
               Quick_Synth.Trig (Button);
            end if;
         when Play_And_Edit | Edit =>
            Toggle (Sequences (Pattern_Sequencer.Current_Pattern)(Current_Track),
                    To_Value (Button));
      end case;
   end On_Press;

   ----------------
   -- On_Release --
   ----------------

   procedure On_Release (Button : Keyboard_Button) is
   begin
      null;
   end On_Release;

   --------------------
   -- Select_Channel --
   --------------------

   procedure Select_Track (Track : Tracks) is
   begin
      Current_Track := Track;
   end Select_Track;

   -------------------------
   -- Copy_Current_Patern --
   -------------------------

   procedure Copy_Current_Patern (To : Patterns) is
   begin
      Sequences (To) := Sequences (Pattern_Sequencer.Current_Pattern);
   end Copy_Current_Patern;

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

   ----------------
   -- Change_BPM --
   ----------------

   procedure Change_BPM (BPM_Delta : Integer) is
      Res : Integer;
   begin
      Res := Integer (Sequencer_BPM) + BPM_Delta;
      if Res in Beat_Per_Minute then
         Sequencer_BPM := Res;
      end if;
   end Change_BPM;

   ---------
   -- BPM --
   ---------

   function BPM return Beat_Per_Minute
   is (Sequencer_BPM);

   ------------
   -- Random --
   ------------

   function Random return Rand_Percent is
      T : UInt32;
   begin
      Rand_X := Rand_X xor Shift_Left (Rand_X, 16);
      Rand_X := Rand_X xor Shift_Right (Rand_X, 5);
      Rand_X := Rand_X xor Shift_Left (Rand_X, 1);

      T := Rand_X;
      Rand_X := Rand_Y;
      Rand_Y := Rand_Z;
      Rand_Z := T xor Rand_X xor Rand_Y;

      return Rand_Percent (Rand_Z mod 100);
   end Random;

   ------------------
   -- Process_Step --
   ------------------

   procedure Process_Step (Pattern : Patterns; Step : Sequencer_Steps) is
   begin
      for Track in Tracks loop
         case Trig (Sequences (Pattern)(Track), Step) is
            when None =>
               null;
            when Always =>
               Quick_Synth.Trig (Track);
            when Percent_25 =>
               if Random <= 25 then
                  Quick_Synth.Trig (Track);
               end if;
            when Percent_50 =>
               if Random <= 50 then
                  Quick_Synth.Trig (Track);
               end if;
            when Percent_75 =>
               if Random <= 75 then
                  Quick_Synth.Trig (Track);
               end if;
         end case;
      end loop;
   end Process_Step;

   ------------------
   -- Execute_Step --
   ------------------

   procedure Execute_Step is
      Now : constant Time := Clock;
   begin

      if Now < Next_Start then
         --  Nothing to do yet...
         return;
      end if;

      Next_Start := Next_Start +
        (Milliseconds ((60 * 1000) / Sequencer_BPM) / Steps_Per_Beat) / 2;

      case Microstep is

         --  Begining of a new step
         when 0 =>
            if Current_Step /= Sequencer_Steps'Last then
               Current_Step := Current_Step + 1;
            else
               Current_Step := Sequencer_Steps'First;
               Pattern_Sequencer.Signal_End_Of_Pattern;
            end if;

         --  At the middle of the step we play the recorded notes
         when 1 =>
            if Current_Seq_State in Play | Play_And_Rec | Play_And_Edit then
               Process_Step (Pattern_Sequencer.Current_Pattern, Current_Step);
            end if;
      end case;
      Microstep := Microstep + 1;
   end Execute_Step;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin

      for Pattern in Patterns loop
         for Track in Tracks loop
            Clear (Sequences (Pattern)(Track));
         end loop;
      end loop;
      Next_Start := Clock + Milliseconds (100);

      Ada.Synchronous_Task_Control.Set_True (Task_Start);
   end Start;


   --------------------
   -- Sequencer_Task --
   --------------------

   task body Sequencer_Task is
   begin
      Ada.Synchronous_Task_Control.Suspend_Until_True (Task_Start);
      loop
         delay until Next_Start;
         Execute_Step;
      end loop;
   end Sequencer_Task;

   ---------
   -- Set --
   ---------

   function Set (Step : Sequencer_Steps) return Boolean is
   begin
      return Trig (Sequences (Pattern_Sequencer.Current_Pattern)(Current_Track),
                   Step) /= None;
   end Set;

   ---------
   -- Set --
   ---------

   function Set (Track : Tracks; Step : Sequencer_Steps) return Boolean is
   begin
      return Trig (Sequences (Pattern_Sequencer.Current_Pattern)(Track), Step) /= None;
   end Set;

   ----------
   -- Trig --
   ----------

   function Trig (Step : Sequencer_Steps) return Trigger is
   begin
      return Trig (Sequences (Pattern_Sequencer.Current_Pattern)(Current_Track),
                   Step);
   end Trig;

   ---------------
   -- Trig_Next --
   ---------------

   procedure Trig_Next (Step : Sequencer_Steps) is
   begin
      Next (Sequences (Pattern_Sequencer.Current_Pattern)(Current_Track), Step);
   end Trig_Next;

   ---------------
   -- Trig_Prev --
   ---------------

   procedure Trig_Prev (Step : Sequencer_Steps) is
   begin
      Previous (Sequences (Pattern_Sequencer.Current_Pattern)(Current_Track), Step);
   end Trig_Prev;

end WNM.Sequencer;
