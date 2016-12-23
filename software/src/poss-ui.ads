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

with STM32.Device; use STM32.Device;
with STM32.GPIO;   use STM32.GPIO;

package POSS.UI is

   procedure Start;


   function Is_Pressed (B : Buttons) return Boolean;

   procedure Turn_On (B : LEDs);
   procedure Turn_Off (B : LEDs);

private

   Key_To_Point : constant array (Buttons) of GPIO_Point :=
     (B1      => PC15,
      B2      => PC13,
      B3      => PE4,
      B4      => PB8,
      B5      => PB5,
      B6      => PD7,
      B7      => PD6,
      B8      => PD1,
      B9      => PB7,
      B10     => PC14,
      B11     => PE6,
      B12     => PE5,
      B13     => PE2,
      B14     => PB4,
      B15     => PB3,
      B16     => PD3,
      Rec     => PD2,
      Play    => PD0,
      FX      => PD11,
      BPM_Vol => PD9,
      Chan_A  => PC1,
      Chan_B  => PC2,
      Chan_C  => PD8,
      Chan_D  => PB15,
      Chan_E  => PD10);

   type Row_Index is range 1 .. 5;
   type Col_Index is range 1 .. 5;

   Row_To_Point : array (Row_Index) of GPIO_Point :=
     (1 => PC9,
      2 => PC8, --  PA13 in RevA, conflict with debugger...
      3 => PA15,
      4 => PC11,
      5 => PA8);

   Col_To_Point : array (Col_Index) of GPIO_Point :=
     (1 => PC6, --  PA14 in RevA, conflict with debugger...
      2 => PA10,
      3 => PA2,
      4 => PC3,
      5 => PC0);

   type LED_Address is record
      Row : Row_Index;
      Col : Col_Index;
   end record;

   Key_To_LED : constant array (Buttons) of LED_Address :=
     (B1      => (Row => 5, Col => 4),
      B2      => (Row => 4, Col => 4),
      B3      => (Row => 3, Col => 4),
      B4      => (Row => 2, Col => 4),
      B5      => (Row => 1, Col => 2),
      B6      => (Row => 2, Col => 2),
      B7      => (Row => 3, Col => 2),
      B8      => (Row => 4, Col => 2),
      B9      => (Row => 5, Col => 5),
      B10     => (Row => 4, Col => 5),
      B11     => (Row => 3, Col => 5),
      B12     => (Row => 2, Col => 5),
      B13     => (Row => 1, Col => 1),
      B14     => (Row => 2, Col => 1),
      B15     => (Row => 3, Col => 1),
      B16     => (Row => 4, Col => 1),
      Rec     => (Row => 5, Col => 1),
      Play    => (Row => 5, Col => 2),
      FX      => (Row => 5, Col => 3),
      BPM_Vol => (Row => 4, Col => 3),
      Chan_A  => (Row => 1, Col => 4),
      Chan_B  => (Row => 1, Col => 5),
      Chan_C  => (Row => 1, Col => 3),
      Chan_D  => (Row => 2, Col => 3),
      Chan_E  => (Row => 3, Col => 3));

   LED_State : array (Buttons) of Boolean := (others => False);

   type Buttton_Event is (On_Press,
                          On_Long_Press,
                          On_Release,
                          Waiting_For_Long_Press);
   type Raw_Key_State is (Up, Down);

   Key_State : array (Buttons) of Raw_Key_State := (others => Up);
   --  FIXME: This array stays here for external access to the button state.
   --  In the future we shouldn't have to export this info, the array could
   --  then be moved in UI_Task.

   Has_Long_Press : constant array (Buttons) of Boolean :=
   --  Can this button trigger a On_Long_Press event?
     (B1      => False,
      B2      => False,
      B3      => False,
      B4      => False,
      B5      => False,
      B6      => False,
      B7      => False,
      B8      => False,
      B9      => False,
      B10     => False,
      B11     => False,
      B12     => False,
      B13     => False,
      B14     => False,
      B15     => False,
      B16     => False,
      Rec     => True,
      Play    => False,
      FX      => False,
      BPM_Vol => True,
      Chan_A  => True,
      Chan_B  => True,
      Chan_C  => True,
      Chan_D  => True,
      Chan_E  => True);

end POSS.UI;
