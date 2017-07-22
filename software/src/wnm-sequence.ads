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

with MIDI;

package WNM.Sequence is


   type Instance is private;
   type Ref is access all Instance;

   procedure Clear (This : in out Instance);

   procedure Add (This : in out Instance;
                  Step : Sequencer_Steps;
                  Cmd : MIDI.Command);

   procedure Remove (This : in out Instance;
                     Step : Sequencer_Steps;
                     Evt  : MIDI.Command);

   function Last_Index (This : Instance;
                        Step : Sequencer_Steps)
                        return Natural
     with Inline_Always;

   function Cmd (This  : Instance;
                 Step  : Sequencer_Steps;
                 Index : Positive) return MIDI.Command
     with Inline_Always;

private


   subtype Event_Count is Natural range 0 .. Max_Events_Per_Step;
   subtype Event_Index is Event_Count range 1 .. Event_Count'Last;

   type Event_Array is array (Sequencer_Steps, Event_Index)
     of MIDI.Command with Pack;
   type Event_Count_Array is array (Sequencer_Steps) of Event_Count;

   type Instance is record
      Cnt    : Event_Count_Array := (others => 0);
      Events : Event_Array;
   end record;

end WNM.Sequence;
