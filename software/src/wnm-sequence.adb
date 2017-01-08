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
      This.Cnt := 0;
   end Clear;

   ---------
   -- Add --
   ---------

   procedure Add (This : in out Instance; Evt : Event) is
   begin
      if This.Cnt < This.Events'Last then
         for Index in This.Events'First .. This.Cnt loop
            if This.Events (Index) = Evt then
               return;
            end if;
         end loop;
         This.Cnt := This.Cnt + 1;
         This.Events (This.Cnt) := Evt;
      end if;
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove (This : in out Instance; Evt : Event) is
   begin
      if This.Cnt /= 0 then
         for Index in This.Events'First .. This.Cnt loop
            if This.Events (Index) = Evt then
               for Del in Index .. This.Cnt - 1 loop
                  This.Events (Del) := This.Events (Del + 1);
               end loop;
               This.Cnt := This.Cnt - 1;
               return;
            end if;
         end loop;
      end if;
   end Remove;

   ----------
   -- List --
   ----------

   function List (This : Instance) return Event_Array is
     (This.Events (1 .. This.Cnt));

end WNM.Sequence;
