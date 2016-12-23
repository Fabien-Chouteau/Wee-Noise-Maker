-------------------------------------------------------------------------------
--                                                                           --
--                       Pocket Open Source Synthesizer                      --
--                                                                           --
--                     Copyright (C) 2016 Fabien Chouteau                    --
--                                                                           --
--    POSS is free software: you can redistribute it and/or modify it        --
--    under the terms of the GNU General Public License as published by      --
--    the Free Software Foundation, either version 3 of the License, or      --
--    (at your option) any later version.                                    --
--                                                                           --
--    POSS is distributed in the hope that it will be useful, but WITHOUT    --
--    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY     --
--    or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public        --
--    License for more details.                                              --
--                                                                           --
--    You should have received a copy of the GNU General Public License      --
--    along with POSS. If not, see <http://www.gnu.org/licenses/>.           --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;

package body POSS.Sequencer is

   Start_Sequencer_Task : Suspension_Object;
   Sequencer_Task_Period : Time_Span := Seconds (1);

   Current_Step      : Sequencer_Steps := Sequencer_Steps'First with Atomic;
   Current_Seq_State : Sequencer_State := Pause with Atomic;
   Current_Chan      : Channels := Channels'First;
   Instrument        : array (Channels) of Keyboard_Value :=
     (others => Keyboard_Value'First);
   pragma Unreferenced (Instrument);

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
      pragma Unreferenced (Button);
   begin
      Sequences (Current_Chan).Add ((Int => 10));
   end On_Press;

   ----------------
   -- On_Release --
   ----------------

   procedure On_Release (Button : Keyboard_Buttons) is
   begin
      raise Program_Error with "Unimplemented procedure On_Release";
   end On_Release;

   --------------------
   -- Select_Channel --
   --------------------

   procedure Select_Channel (Chan : Channels) is
   begin
      Current_Chan := Chan;
   end Select_Channel;

   --------------------
   -- Set_Instrument --
   --------------------

   procedure Set_Instrument (Val : Keyboard_Value) is
   begin
      Instrument (Current_Chan) := Val;
   end Set_Instrument;

   ---------------------
   -- Set_Beat_Period --
   ---------------------

   procedure Set_Beat_Period (Period : Time_Span) is
   begin
      Sequencer_Task_Period := Period;
   end Set_Beat_Period;

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
      Period     : Time_Span renames Sequencer_Task_Period;
   begin
      Suspend_Until_True (Start_Sequencer_Task);
      Next_Start := Clock;
      loop
         Next_Start := Next_Start + Period / Steps_Per_Beat;
         delay until Next_Start;

         if Current_Step /= Sequencer_Steps'Last then
            Current_Step := Current_Step + 1;
         else
            Current_Step := Sequencer_Steps'First;
         end if;
      end loop;
   end Sequencer_Task;

end POSS.Sequencer;
