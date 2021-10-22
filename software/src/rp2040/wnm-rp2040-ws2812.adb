-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2021 Fabien Chouteau                    --
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

with RP.Device;
with RP.PIO; use RP.PIO;
with RP.GPIO; use RP.GPIO;
with WNM.RP2040.WS2812_PIO_ASM;

package body WNM.RP2040.WS2812 is

   PIO : PIO_Device renames RP.Device.PIO_0;

   Out_Pin : RP.GPIO.GPIO_Point := (Pin => 3);
   SM      : constant PIO_SM := 0;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Config         : PIO_SM_Config := Default_SM_Config;

      Freq           : constant := 80_0000;
      Cycles_Per_Bit : constant := WS2812_PIO_ASM.T1 +
        WS2812_PIO_ASM.T2 + WS2812_PIO_ASM.T3;

      Bit_Per_LED : constant := 24;
   begin
      Out_Pin.Configure (Output, Pull_Up, Pio.GPIO_Function);

      PIO.Enable;
      PIO.Load (WS2812_PIO_ASM.Ws2812_Program_Instructions,
                Offset => 0);

      PIO.Set_Pin_Direction (SM, Out_Pin.Pin, Output);

      Set_Sideset (Config,
                   Bit_Count => 1,
                   Optional  => False,
                   Pindirs   => False);
      Set_Sideset_Pins (Config, Sideset_Base => Out_Pin.Pin);

      Set_Out_Shift (Config,
                     Shift_Right    => True,
                     Autopull       => True,
                     Pull_Threshold => Bit_Per_LED);
      Set_FIFO_Join (Config,
                     Join_TX => True,
                     Join_RX => False);

      Set_Wrap (Config,
                WS2812_PIO_ASM.Ws2812_Wrap_Target,
                WS2812_PIO_ASM.Ws2812_Wrap);
      Set_Clock_Frequency (Config, Freq * Cycles_Per_Bit);

      PIO.SM_Initialize (SM, 0, Config);
      PIO.Set_Enabled (SM, True);
   end Initialize;

   -------------------
   -- Push_Data_DMA --
   -------------------

   procedure Push_Data_DMA (Data : not null LED_Data_Access) is
   begin
      for Elt of Data.all loop
         PIO.Put (SM, HAL.UInt32 (Elt));
      end loop;
   end Push_Data_DMA;

begin
   Initialize;
end WNM.RP2040.WS2812;
