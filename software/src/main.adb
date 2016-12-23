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

with Ada.Real_Time; use Ada.Real_Time;

with Test_I2S;
with POSS.Sequencer;
with POSS.UI;

procedure Main is
   pragma Priority (POSS.Synth_Task_Priority);
begin

   POSS.Sequencer.Start;
   POSS.UI.Start;

   Test_I2S.Init;
   Test_I2S.Play;

   delay until Time_Last;

end Main;
