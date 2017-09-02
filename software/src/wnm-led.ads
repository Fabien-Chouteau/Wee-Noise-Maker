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
with Ada.Interrupts.Names;

with STM32.Timers; use STM32.Timers;
with STM32.PWM;    use STM32.PWM;
with STM32.GPIO;   use STM32.GPIO;
with STM32.Device; use STM32.Device;
with HAL;          use HAL;

package WNM.LED is

   procedure Start;
   procedure Turn_On (B : LEDs) with Inline_Always;
   procedure Turn_Off (B : LEDs) with Inline_Always;
   procedure Turn_Off_All with Inline_Always;

private

   type Row_Index is range 1 .. 3;
   type Col_Index is range 1 .. 9;

   Row_To_Point : array (Row_Index) of GPIO_Point :=
     (1 => PD13,
      2 => PD12,
      3 => PD11);

   Col_To_Point : array (Col_Index) of GPIO_Point :=
     (1 => PC0,
      2 => PC2,
      3 => PA4,
      4 => PB0,
      5 => PE9,
      6 => PE13,
      7 => PB11,
      8 => PB15,
      9 => PD14);

   type LED_Address is record
      Row : Row_Index;
      Col : Col_Index;
   end record;

   LED_To_Address : constant array (LEDs) of LED_Address :=
     (B1      => (Row => 2, Col => 1),
      B2      => (Row => 2, Col => 2),
      B3      => (Row => 2, Col => 3),
      B4      => (Row => 2, Col => 4),
      B5      => (Row => 2, Col => 5),
      B6      => (Row => 2, Col => 6),
      B7      => (Row => 2, Col => 7),
      B8      => (Row => 2, Col => 8),
      B9      => (Row => 1, Col => 1),
      B10     => (Row => 1, Col => 2),
      B11     => (Row => 1, Col => 3),
      B12     => (Row => 1, Col => 4),
      B13     => (Row => 1, Col => 5),
      B14     => (Row => 1, Col => 6),
      B15     => (Row => 1, Col => 7),
      B16     => (Row => 1, Col => 8),
      Rec     => (Row => 1, Col => 9),
      Play    => (Row => 2, Col => 9),
      FX      => (Row => 3, Col => 9),
      Track_A  => (Row => 3, Col => 4),
      Track_B  => (Row => 3, Col => 5),
      Track_C  => (Row => 3, Col => 6),
      Track_D  => (Row => 3, Col => 7),
      Track_E  => (Row => 3, Col => 8));

   LED_State : array (Buttons) of UInt8 := (others => 0);

   LED_Timer : STM32.Timers.Timer renames Timer_7;
   LED_Timer_Control : PWM_Modulator;

   protected LED_Timer_Handler is
      pragma Interrupt_Priority;

   private

      Current_LED : LEDs := Buttons'First;

      procedure IRQ_Handler;
      pragma Attach_Handler (IRQ_Handler, Ada.Interrupts.Names.TIM7_Interrupt);

   end LED_Timer_Handler;

end WNM.LED;
