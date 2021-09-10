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

with WNM.Time;

with WNM.Sequence_Copy;

package WNM.UI is

   function Update return Time.Time_Ms;

   type Input_Mode_Type is (Note,
                            Volume_BPM,
                            FX_Alt,
                            Copy,
                            Track_Select,
                            Pattern_Select,
                            Step_Edit);

   function Input_Mode return Input_Mode_Type;

   function Current_Editting_Trig return Sequencer_Steps;

   function FX_On (B : Keyboard_Button) return Boolean;

   Copy_T : WNM.Sequence_Copy.Copy_Transaction;

private

   type Buttton_Event is (On_Press,
                          On_Long_Press,
                          On_Release,
                          Waiting_For_Long_Press);

   function Has_Long_Press (B : Button) return Boolean;
   --  Can this button trigger a On_Long_Press event?

end WNM.UI;
