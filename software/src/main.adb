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

with Ada.Real_Time; use Ada.Real_Time;

with WNM.Sequencer;
with WNM.UI;
with WNM.GUI;
with WNM.Master_Volume;
with WNM.SDCard;
with WNM.Sample_Stream;
with WNM.Sample_Library;

procedure Main is
begin

   WNM.SDCard.Initialize;

   WNM.Sample_Library.Load;

   WNM.Sample_Stream.Start_Sample_Stream_Task;
   WNM.UI.Start;
   WNM.Sequencer.Start;

   loop
      WNM.Master_Volume.Update;
      WNM.GUI.Update;
      delay until Clock + Milliseconds (100);
   end loop;
end Main;
