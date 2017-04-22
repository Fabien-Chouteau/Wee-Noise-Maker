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

with STM32.Device;                 use STM32.Device;
with STM32.GPIO;                   use STM32.GPIO;
with STM32.I2C;                    use STM32.I2C;
with Ravenscar_Time;

package body WNM.I2C is

   I2C_Port : STM32.I2C.I2C_Port renames I2C_1;
   Init_Done : Boolean := False;

   procedure Initialize_I2C_GPIO;
   procedure Configure_I2C;

   -------------------------
   -- Initialize_I2C_GPIO --
   -------------------------

   procedure Initialize_I2C_GPIO is
      Points : GPIO_Points (1 .. 2);
   begin
      Points := (PB6, PB7);

      Enable_Clock (Points);

      Configure_Alternate_Function (Points, GPIO_AF_4_I2C1);

      Configure_IO (Points,
                    (Speed       => Speed_High,
                     Mode        => Mode_AF,
                     Output_Type => Open_Drain,
                     Resistors   => Floating));
      --  Lock (Points);
   end Initialize_I2C_GPIO;

   -------------------
   -- Configure_I2C --
   -------------------

   procedure Configure_I2C
   is
      I2C_Conf : I2C_Configuration;
   begin

      Enable_Clock (I2C_Port);
      Ravenscar_Time.Delays.Delay_Milliseconds (200);
      Reset (I2C_Port);

      I2C_Conf.Own_Address := 16#00#;
      I2C_Conf.Addressing_Mode := Addressing_Mode_7bit;
      I2C_Conf.General_Call_Enabled := False;
      I2C_Conf.Clock_Stretching_Enabled := True;

      I2C_Conf.Clock_Speed := 800_000;

      I2C_Port.Configure (I2C_Conf);
   end Configure_I2C;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
   is
   begin
      Initialize_I2C_GPIO;
      Configure_I2C;
      Init_Done := True;
   end Initialize;

   -----------------
   -- Initialized --
   -----------------

   function Initialized return Boolean
   is (Init_Done);

   ----------
   -- Port --
   ----------

   function Port
      return not null HAL.I2C.Any_I2C_Port
   is
   begin
      return I2C_Port'Access;
   end Port;

end WNM.I2C;
