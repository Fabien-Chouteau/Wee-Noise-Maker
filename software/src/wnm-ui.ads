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


with STM32.Device; use STM32.Device;
with STM32.GPIO;   use STM32.GPIO;
with HAL;          use HAL;

package WNM.UI is

   procedure Start;

   type Input_Mode_Type is (Note,
                            Volume_BPM,
                            FX_Select,
                            Track_Select,
                            Pattern_Select,
                            Pattern_Copy,
                            Trig_Edit);

   function Input_Mode return Input_Mode_Type;

   function Is_Pressed (B : Buttons) return Boolean;

   function Current_Editting_Trig return Sequencer_Steps
     with Pre => Input_Mode = Trig_Edit;

private

   type Row_Index is range 1 .. 3;
   type Col_Index is range 1 .. 11;

   Row_To_Point : array (Row_Index) of GPIO_Point :=
     (1 => PB12,
      2 => PD14,
      3 => PA2);

   Col_To_Point : array (Col_Index) of GPIO_Point :=
     (1  => PC4,
      2  => PB0,
      3  => PE7,
      4  => PE9,
      5  => PE11,
      6  => PE13,
      7  => PE15,
      8  => PB11,
      9  => PB14,
      10 => PB15,
      11 => PD10);

   type Key_Address is record
      Row : Row_Index;
      Col : Col_Index;
   end record;

   Key_To_Address : constant array (Buttons) of Key_Address :=
     (B1        => (Row => 1, Col => 2),
      B2        => (Row => 1, Col => 3),
      B3        => (Row => 1, Col => 4),
      B4        => (Row => 1, Col => 5),
      B5        => (Row => 1, Col => 6),
      B6        => (Row => 1, Col => 7),
      B7        => (Row => 1, Col => 8),
      B8        => (Row => 1, Col => 9),
      B9        => (Row => 2, Col => 2),
      B10       => (Row => 2, Col => 3),
      B11       => (Row => 2, Col => 4),
      B12       => (Row => 2, Col => 5),
      B13       => (Row => 2, Col => 6),
      B14       => (Row => 2, Col => 7),
      B15       => (Row => 2, Col => 8),
      B16       => (Row => 2, Col => 9),
      Rec       => (Row => 2, Col => 10),
      Play      => (Row => 1, Col => 1),  -- Play is actually not connected to the matrix...
      Menu      => (Row => 1, Col => 10),
      Func      => (Row => 2, Col => 11),
      Track_Button     => (Row => 1, Col => 1),
      Pattern   => (Row => 2, Col => 1),
      Encoder_L => (Row => 3, Col => 1),
      Encoder_R => (Row => 3, Col => 3));

   Wakeup : GPIO_Point renames PA0;

   type Buttton_Event is (On_Press,
                          On_Long_Press,
                          On_Release,
                          Waiting_For_Long_Press);
   type Raw_Key_State is (Up, Down);

   Key_State : array (Buttons) of Raw_Key_State := (others => Up);

   function Has_Long_Press (Button : Buttons) return Boolean;
   --  Can this button trigger a On_Long_Press event?

end WNM.UI;
