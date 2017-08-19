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

package body WNM.Sequence is

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Instance) is
   begin
      This.Events := (others => None);
   end Clear;

   ---------
   -- Set --
   ---------

   procedure Set (This : in out Instance;
                  Step : Sequencer_Steps;
                  Trig : Trigger := Always)
   is
   begin
      This.Events (Step) := Trig;
   end Set;

   ------------
   -- Toggle --
   ------------

   procedure Toggle (This : in out Instance;
                     Step : Sequencer_Steps)
   is
   begin
      if This.Events (Step) /= None then
         This.Events (Step) := None;
      else
         This.Events (Step) := Always;
      end if;
   end Toggle;

   ----------
   -- Next --
   ----------

   procedure Next (This : in out Instance;
                   Step : Sequencer_Steps)
   is
   begin
      if This.Events (Step) = Trigger'Last then
         This.Events (Step) := Trigger'First;
      else
         This.Events (Step) := Trigger'Succ (This.Events (Step));
      end if;
   end Next;

   --------------
   -- Previous --
   --------------

   procedure Previous (This : in out Instance;
                       Step : Sequencer_Steps)
   is
   begin
      if This.Events (Step) = Trigger'First then
         This.Events (Step) := Trigger'Last;
      else
         This.Events (Step) := Trigger'Pred (This.Events (Step));
      end if;
   end Previous;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Instance;
                    Step : Sequencer_Steps)
   is
   begin
      This.Events (Step) := None;
   end Clear;

   ----------
   -- Trig --
   ----------

   function Trig (This  : Instance;
                  Step  : Sequencer_Steps)
                 return Trigger
   is
   begin
      return This.Events (Step);
   end Trig;

end WNM.Sequence;
