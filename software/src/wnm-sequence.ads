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

package WNM.Sequence is


   type Instance is private;
   type Ref is access all Instance;

   procedure Clear (This : in out Instance)
     with Inline_Always;

   procedure Set (This : in out Instance;
                  Step : Sequencer_Steps;
                  Trig : Trigger := Always)
     with Inline_Always;

   procedure Toggle (This : in out Instance;
                     Step : Sequencer_Steps)
     with Inline_Always;

   procedure Next (This : in out Instance;
                   Step : Sequencer_Steps)
     with Inline_Always;

   procedure Previous (This : in out Instance;
                       Step : Sequencer_Steps)
     with Inline_Always;

   procedure Clear (This : in out Instance;
                    Step : Sequencer_Steps)
     with Inline_Always;

   function Trig (This  : Instance;
                 Step  : Sequencer_Steps)
                 return Trigger
     with Inline_Always;

private

   type Event_Array is array (Sequencer_Steps) of Trigger with Pack;

   type Instance is record
      Events : Event_Array;
   end record;

end WNM.Sequence;
