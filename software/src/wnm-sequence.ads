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

   type Event is record
      Int : Integer;
   end record;

   type Event_Array is array (Positive range <>) of Event;

   type Instance is tagged private;
   subtype Class is Instance'Class;
   type Ref is access all Class;

   procedure Clear (This : in out Instance);
   procedure Add (This : in out Instance; Evt : Event);
   procedure Remove (This : in out Instance; Evt : Event);
   function List (This : Instance) return Event_Array;

private

   subtype Event_Count is Natural range 0 .. Max_Events_Per_Step;
   subtype Event_Index is Event_Count range 1 .. Event_Count'Last;

   type Instance is tagged record
      Cnt    : Event_Count := 0;
      Events : Event_Array (Event_Index);
   end record;

end WNM.Sequence;
