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

--  with MIDI;
with WNM.Short_Term_Sequencer;
with WNM.Pattern_Sequencer;
with WNM.UI;
with HAL;                   use HAL;

package body WNM.Sequencer is

   --  use type MIDI.Octaves;

   type Step_Rec is record
      Trig        : Trigger;
      Repeat      : WNM.Repeat;
      Repeat_Rate : WNM.Repeat_Rate;
   end record;

   type Sequence is array (Sequencer_Steps) of Step_Rec with Pack;
   type Pattern is array (Tracks) of Sequence;
   Sequences : array (Patterns) of Pattern;

   Sequencer_BPM : Beat_Per_Minute := 90;

   Current_Step      : Sequencer_Steps := Sequencer_Steps'First with Atomic;
   Current_Seq_State : Sequencer_State := Pause with Atomic;
   Current_Track     : Tracks := Tracks'First with Atomic;
   Track_Instrument  : array (Tracks) of Keyboard_Value :=
     (others => Keyboard_Value'First);

   Next_Start : Synth.Sample_Time := Synth.Sample_Time'First;

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
      if Current_Seq_State in Play | Play_And_Rec | Edit | Play_And_Edit then
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
            WNM.Synth.Trig (Button);
            Current_Track := Button;
         when Play_And_Rec =>
            Sequences (Pattern_Sequencer.Current_Pattern)(Button)(Step).Trig := Always;
            Current_Track := Button;
            if Microstep /= 1 then
               WNM.Synth.Trig (Button);
            end if;
         when Play_And_Edit | Edit =>

            declare
               S : Step_Rec renames Sequences (Pattern_Sequencer.Current_Pattern)
                                              (Current_Track)
                 (To_Value (Button));
            begin
               if S.Trig /= None then
                  S.Trig := None;
               else
                  S.Trig := Always;
               end if;
            end;
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

   ----------------------
   -- Samples_Per_Beat --
   ----------------------

   function Samples_Per_Beat return Synth.Sample_Time is
      use Synth;

      Samples_Per_Minute : constant Sample_Time := 60 * Sample_Frequency;
   begin
      return Samples_Per_Minute / Sample_Time (Sequencer_BPM);
   end Samples_Per_Beat;

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

   procedure Process_Step (Pattern : Patterns;
                           Step : Sequencer_Steps)
   is
      use Synth;

      Condition : Boolean := False;
      Now : constant Sample_Time := Sample_Clock;
   begin
      for Track in Tracks loop
         case Sequences (Pattern)(Track)(Step).Trig is
            when None =>
               Condition := False;
            when Always =>
               Condition := True;
            when Fill =>
               Condition := WNM.UI.FX_On (B1);
            when Percent_25 =>
               condition := Random <= 25;
            when Percent_50 =>
               condition := Random <= 50;
            when Percent_75 =>
               condition := Random <= 75;
         end case;

         if Condition then
            WNM.Synth.Trig (Track);

            declare
               Repeat_Span : constant Sample_Time :=
                 Samples_Per_Beat /  (case Sequences (Pattern) (Track) (Step).Repeat_Rate is
                                         when Rate_1_1  => 1,
                                         when Rate_1_2  => 2,
                                         when Rate_1_3  => 3,
                                         when Rate_1_4  => 4,
                                         when Rate_1_5  => 5,
                                         when Rate_1_6  => 6,
                                         when Rate_1_8  => 8,
                                         when Rate_1_10 => 10,
                                         when Rate_1_12 => 12,
                                         when Rate_1_16 => 16,
                                         when Rate_1_20 => 20,
                                         when Rate_1_24 => 24,
                                         when Rate_1_32 => 32);
               Repeat_Time : Sample_Time := Now + Repeat_Span;
            begin
               for Rep in 1 .. Sequences (Pattern) (Track) (Step).Repeat loop
                  WNM.Short_Term_Sequencer.Push (Track, Repeat_Time);
                  Repeat_Time := Repeat_Time + Repeat_Span;
               end loop;
            end;
         end if;
      end loop;
   end Process_Step;

   ------------------
   -- Execute_Step --
   ------------------

   procedure Execute_Step is
      use Synth;
   begin

      --  Next_Start := Next_Start +
      --    (Time.Time_Ms ((60 * 1000) / Sequencer_BPM) / Steps_Per_Beat) / 2;

      Next_Start := Next_Start + Samples_Per_Beat / Steps_Per_Beat / 2;

      if Current_Seq_State in Play | Play_And_Rec | Play_And_Edit then
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
      end if;
   end Execute_Step;

   ------------
   -- Update --
   ------------

   function Update return Time.Time_Ms is
      use Synth;

      Now     : constant Sample_Time := Sample_Clock;
      Success : Boolean;
      Track   : Tracks;
   begin
      if now >= Next_Start then
         Execute_Step;
      end if;

      loop
         WNM.Short_Term_Sequencer.Pop (Now, Track, Success);
         exit when not Success;

         WNM.Synth.Trig (Track);
      end loop;

      return Time.Time_Ms'First;
   end Update;

   ---------
   -- Set --
   ---------

   function Set (Step : Sequencer_Steps) return Boolean is
   begin
      return Sequences (Pattern_Sequencer.Current_Pattern) (Current_Track) (Step).Trig /= None;
   end Set;

   ---------
   -- Set --
   ---------

   function Set (Track : Tracks; Step : Sequencer_Steps) return Boolean is
   begin
      return Sequences (Pattern_Sequencer.Current_Pattern)(Track)(Step).Trig /= None;
   end Set;

   ----------
   -- Trig --
   ----------

   function Trig (Step : Sequencer_Steps) return Trigger is
   begin
      return Sequences (Pattern_Sequencer.Current_Pattern)(Current_Track)(Step).Trig;
   end Trig;

   ---------------
   -- Trig_Next --
   ---------------

   procedure Trig_Next (Step : Sequencer_Steps) is
   begin
      Next (Sequences (Pattern_Sequencer.Current_Pattern)(Current_Track)(Step).Trig);
   end Trig_Next;

   ---------------
   -- Trig_Prev --
   ---------------

   procedure Trig_Prev (Step : Sequencer_Steps) is
   begin
      Prev (Sequences (Pattern_Sequencer.Current_Pattern)(Current_Track)(Step).Trig);
   end Trig_Prev;

   ------------
   -- Repeat --
   ------------

   function Repeat (Step : Sequencer_Steps) return WNM.Repeat is
   begin
      return Sequences (Pattern_Sequencer.Current_Pattern)(Current_Track)(Step).Repeat;
   end Repeat;

   -----------------
   -- Repeat_Next --
   -----------------

   procedure Repeat_Next (Step : Sequencer_Steps) is
   begin
      Next (Sequences (Pattern_Sequencer.Current_Pattern)(Current_Track)(Step).Repeat);
   end Repeat_Next;

   -----------------
   -- Repeat_Prev --
   -----------------

   procedure Repeat_Prev (Step : Sequencer_Steps) is
   begin
      Prev (Sequences (Pattern_Sequencer.Current_Pattern)(Current_Track)(Step).Repeat);
   end Repeat_Prev;

   -----------------
   -- Repeat_Rate --
   -----------------

   function Repeat_Rate (Step : Sequencer_Steps) return WNM.Repeat_Rate is
   begin
      return Sequences (Pattern_Sequencer.Current_Pattern) (Current_Track) (Step).Repeat_Rate;
   end Repeat_Rate;


   ----------------------
   -- Repeat_Rate_Next --
   ----------------------

   procedure Repeat_Rate_Next (Step : Sequencer_Steps) is
   begin
      Next (Sequences (Pattern_Sequencer.Current_Pattern)(Current_Track)(Step).Repeat_Rate);
   end Repeat_Rate_Next;

   ----------------------
   -- Repeat_Rate_Prev --
   ----------------------

   procedure Repeat_Rate_Prev (Step : Sequencer_Steps) is
   begin
      Prev (Sequences (Pattern_Sequencer.Current_Pattern)(Current_Track)(Step).Repeat_Rate);
   end Repeat_Rate_Prev;


begin
   for Pattern in Patterns loop
      for Track in Tracks loop
         for Step in Sequencer_Steps loop
            Sequences (Pattern) (Track) (Step) := (None, 0, Rate_1_1);
         end loop;
      end loop;
   end loop;
end WNM.Sequencer;
