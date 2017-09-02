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
                            Trig_Edit);

   function Input_Mode return Input_Mode_Type;

   function Is_Pressed (B : Buttons) return Boolean;

   function Current_Editting_Trig return Sequencer_Steps
     with Pre => Input_Mode = Trig_Edit;

private

   Key_To_Point : constant array (Buttons) of GPIO_Point :=
     (B1        => PC1,
      B2        => PA2,
      B3        => PC4,
      B4        => PE7,
      B5        => PE11,
      B6        => PE15,
      B7        => PB13,
      B8        => PD9,
      B9        => PC3,
      B10       => PA3,
      B11       => PC5,
      B12       => PE8,
      B13       => PE12,
      B14       => PB10,
      B15       => PB12,
      B16       => PD10,
      Rec       => PA10,
      Play      => PC6,
      FX        => PD15,
      Track_A    => PB1,
      Track_B    => PE10,
      Track_C    => PE14,
      Track_D    => PB14,
      Track_E    => PD8,
      Encoder_L => PC14,
      Encoder_R => PC15);

   Wakeup : GPIO_Point renames PA0;

   type Buttton_Event is (On_Press,
                          On_Long_Press,
                          On_Release,
                          Waiting_For_Long_Press);
   type Raw_Key_State is (Up, Down);

   Key_State : array (Buttons) of Raw_Key_State := (others => Up);
   --  FIXME: This array stays here for external access to the button state.
   --  In the future we shouldn't have to export this info, the array could
   --  then be moved in UI_Task.

   function Has_Long_Press (Button : Buttons) return Boolean;
   --  Can this button trigger a On_Long_Press event?

end WNM.UI;
