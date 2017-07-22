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

with MIDI; use MIDI;

package body WNM.Sequence is

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Instance) is
   begin
      This.Cnt := (others => 0);
   end Clear;

   ---------
   -- Add --
   ---------

   procedure Add (This : in out Instance;
                  Step : Sequencer_Steps;
                  Cmd  : MIDI.Command)
   is
   begin
      if This.Cnt (Step) < This.Events'Last (2) then
         for Index in This.Events'First (2) .. This.Cnt (Step) loop
            if This.Events (Step, Index) = Cmd then
               return;
            end if;
         end loop;
         This.Cnt (Step) := This.Cnt (Step) + 1;
         This.Events (Step, This.Cnt (Step)) := Cmd;
      end if;
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove (This : in out Instance;
                     Step : Sequencer_Steps;
                     Evt  : MIDI.Command)
   is
   begin
      if This.Cnt (Step) /= 0 then
         for Index in This.Events'First (2) .. This.Cnt (Step) loop
            if This.Events (Step, Index) = Evt then
               for Del in Index .. This.Cnt (Step) - 1 loop
                  This.Events (Step, Del) := This.Events (Step, Del + 1);
               end loop;
               This.Cnt (Step) := This.Cnt (Step) - 1;
               return;
            end if;
         end loop;
      end if;
   end Remove;

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index (This : Instance;
                        Step : Sequencer_Steps)
                        return Natural
   is (This.Cnt (Step));

   ---------
   -- Cmd --
   ---------

   function Cmd (This  : Instance;
                 Step  : Sequencer_Steps;
                 Index : Positive) return MIDI.Command
   is
   begin
      return This.Events (Step, Index);
   end Cmd;

end WNM.Sequence;
