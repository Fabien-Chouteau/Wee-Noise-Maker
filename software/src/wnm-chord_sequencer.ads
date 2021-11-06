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

with WNM.MIDI;

package WNM.Chord_Sequencer is

   subtype Tonic is MIDI.MIDI_Key;

   type Interval is (P1, min2, Maj2, min3, Maj3, P4,
                     d5, P5, min6, Maj6, min7, Maj7, Octave);

   for Interval use
     (P1     => 0,
      min2   => 1,
      Maj2   => 2,
      min3   => 3,
      Maj3   => 4,
      P4     => 5,
      d5     => 6,
      P5     => 7,
      min6   => 8,
      Maj6   => 9,
      min7   => 10,
      Maj7   => 11,
      Octave => 12);

   type Chord_Index_Range is range 0 .. 3;
   --  Not trying to model the entire music theory, limiting chords to 4 notes
   --  is good enough here.

   type Chord_Intervals is array (Chord_Index_Range) of Interval;
   type Chord_Notes is array (Chord_Index_Range) of MIDI.MIDI_Key;

   type Scale_Range is range  0 .. 6;
   type Scale is array (Scale_Range) of Interval;

   type Chord_Name is (Maj_Triad, Min_Triad, Dim_Triad, Dom_7th, Min_7th);

   type Scale_Chords_Array is array (Scale_Range) of Chord_Name;
   type Scale_Name is (Major_Scale, Minor_Scale);

   Scale_Chords : constant array (Scale_Name) of Scale_Chords_Array :=
     (Major_Scale => (Maj_Triad, Min_Triad, Min_Triad,
                      Maj_Triad, Maj_Triad, Min_Triad, Dim_Triad),

      Minor_Scale => (Min_Triad, Dim_Triad, Maj_Triad,
                      Min_Triad, Min_Triad, Maj_Triad, Maj_Triad)
     );

   Scale_Intervals : constant array (Scale_Name) of Scale :=
     (Major_Scale => (P1, Maj2, Maj3, P4, P5, Maj6, Maj7),
      Minor_Scale => (P1, Maj2, min3, P4, P5, min6, min7));

   Chords : constant array (Chord_Name) of Chord_Intervals :=
     (Maj_Triad => (P1, Maj3, P5, Octave),
      Min_Triad => (P1, min3, P5, Octave),
      Dim_Triad => (P1, min3, min6, Octave),
      Dom_7th   => (P1, Maj3, P5, min7),
      Min_7th   => (P1, min3, P5, Maj7));
   --  https://en.wikipedia.org/wiki/Chord_(music)

   procedure Play_Pause;
   function Playing return Boolean;

   procedure Signal_End_Of_Pattern;

   function Current_Scale_Key return MIDI.MIDI_Key;
   procedure Scale_Key_Next;
   procedure Scale_Key_Prev;

   function Current_Scale return Scale_Name;
   procedure Scale_Next;
   procedure Scale_Prev;

   function Current_Chord_Index return Scale_Range;
   procedure Chord_Index_Next;
   procedure Chord_Index_Prev;

   function Current_Tonic return MIDI.MIDI_Key;
   function Current_Chord_Intervals return Chord_Intervals;
   function Current_Chord return Chord_Notes;

   function Img (s : Scale_Name) return String
   is (case s is
          when Major_Scale => "Major",
          when Minor_Scale => "Minor");

   package Scale_Name_Nest is new Enum_Next (Scale_Name);
   use Scale_Name_Nest;

   pragma Inline (Current_Scale_Key);
   pragma Inline (Scale_Key_Prev);
   pragma Inline (Scale_Key_Next);
   pragma Inline (Current_Scale);
   pragma Inline (Scale_Next);
   pragma Inline (Scale_Prev);
   pragma Inline (Current_Chord_Index);
   pragma Inline (Current_Tonic);
   pragma Inline (Current_Chord_Intervals);
   pragma Inline (Current_Chord);
end WNM.Chord_Sequencer;
