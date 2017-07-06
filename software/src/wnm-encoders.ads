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

private with STM32.Device;
private with STM32.GPIO;
private with STM32.Timers;

package WNM.Encoders is

   procedure Initialize;

   function Left return Integer;
   function Right return Integer;

private
   use STM32.Device;
   use STM32.GPIO;
   use STM32.Timers;

   Point_L_A : constant GPIO_Point := PA5;
   Point_L_B : constant GPIO_Point := PA1;

   Point_R_A : constant GPIO_Point := PA6;
   Point_R_B : constant GPIO_Point := PA7;

   Timer_L : Timer renames Timer_2;
   Timer_R : Timer renames Timer_3;

   AF_L : constant STM32.GPIO_Alternate_Function := GPIO_AF_TIM2_1;
   AF_R : constant STM32.GPIO_Alternate_Function := GPIO_AF_TIM3_2;

end WNM.Encoders;
