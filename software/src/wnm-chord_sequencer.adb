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

with HAL; use HAL;
with WNM.MIDI; use WNM.MIDI;

package body WNM.Chord_Sequencer is

   Scale_Key : MIDI_Key := MIDI.C4;
   Cur_Scale : Scale_Name := Major_Scale;
   Cur_Chord_Index : Scale_Range := Scale_Range'First;

   ---------
   -- "+" --
   ---------

   function "+"(K : MIDI_Key; I : Interval) return MIDI_Key
   is (K + I'Enum_Rep);

   ---------
   -- "+" --
   ---------

   function "+"(I : Interval; K : MIDI_Key) return MIDI_Key
   is (K + I'Enum_Rep);

   ---------
   -- "+" --
   ---------

   function "+"(K : MIDI_Key; I : Chord_Intervals) return Chord_Notes is
      Result : Chord_Notes;
   begin
      for X in Result'Range loop
         Result (X) := K + I (X);
      end loop;
      return Result;
   end "+";

   ----------------
   -- Play_Pause --
   ----------------

   procedure Play_Pause is
   begin
      raise Program_Error with "Unimplemented procedure Play_Pause";
   end Play_Pause;

   -------------
   -- Playing --
   -------------

   function Playing return Boolean is
   begin
      return raise Program_Error with "Unimplemented function Playing";
   end Playing;

   ---------------------------
   -- Signal_End_Of_Pattern --
   ---------------------------

   procedure Signal_End_Of_Pattern is
   begin
      raise Program_Error with "Unimplemented procedure Signal_End_Of_Pattern";
   end Signal_End_Of_Pattern;

   -----------------------
   -- Current_Scale_Key --
   -----------------------

   function Current_Scale_Key return MIDI.MIDI_Key
   is (Scale_Key);

   --------------------
   -- Scale_Key_Next --
   --------------------

   procedure Scale_Key_Next is
   begin
      if Scale_Key /= MIDI_Key'Last then
         Scale_Key := Scale_Key + 1;
      end if;
   end Scale_Key_Next;

   --------------------
   -- Scale_Key_Prev --
   --------------------

   procedure Scale_Key_Prev is
   begin
      if Scale_Key /= MIDI_Key'First then
         Scale_Key := Scale_Key - 1;
      end if;
   end Scale_Key_Prev;

   -------------------
   -- Current_Scale --
   -------------------

   function Current_Scale return Scale_Name
   is (Cur_Scale);

   ----------------
   -- Scale_Next --
   ----------------

   procedure Scale_Next is
   begin
      Cur_Scale := Next (Cur_Scale);
   end Scale_Next;

   ----------------
   -- Scale_Prev --
   ----------------

   procedure Scale_Prev is
   begin
      Cur_Scale := Prev (Cur_Scale);
   end Scale_Prev;

   -------------------------
   -- Current_Chord_Index --
   -------------------------

   function Current_Chord_Index return Scale_Range
   is (Cur_Chord_Index);

   ----------------------
   -- Chord_Index_Next --
   ----------------------

   procedure Chord_Index_Next is
   begin
      if Cur_Chord_Index /= Scale_Range'Last then
         Cur_Chord_Index := Cur_Chord_Index + 1;
      else
         Cur_Chord_Index := Scale_Range'First;
      end if;
   end Chord_Index_Next;

   ----------------------
   -- Chord_Index_Prev --
   ----------------------

   procedure Chord_Index_Prev is
   begin
      if Cur_Chord_Index /= Scale_Range'First then
         Cur_Chord_Index := Cur_Chord_Index - 1;
      else
         Cur_Chord_Index := Scale_Range'Last;
      end if;
   end Chord_Index_Prev;

   -------------------
   -- Current_Tonic --
   -------------------

   function Current_Tonic return MIDI.MIDI_Key
   is (Current_Scale_Key + Scale_Intervals (Current_Scale) (Current_Chord_Index));

   -----------------------------
   -- Current_Chord_Intervals --
   -----------------------------

   function Current_Chord_Intervals return Chord_Intervals
   is (Chords (Scale_Chords (Current_Scale) (Current_Chord_Index)));

   -------------------
   -- Current_Chord --
   -------------------

   function Current_Chord return Chord_Notes is
   begin
      return Current_Tonic + Current_Chord_Intervals;
   end Current_Chord;

end WNM.Chord_Sequencer;
