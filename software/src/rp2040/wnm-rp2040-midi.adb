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

with WNM.MIDI.Queues;

with RP.Device;
with RP.UART;
with RP.GPIO; use RP.GPIO;

with BBqueue;         use BBqueue;
with BBqueue.Buffers; use BBqueue.Buffers;
with HAL.UART; use HAL.UART;

package body WNM.RP2040.MIDI is

   UART    : RP.UART.UART_Port renames RP.Device.UART_0;
   UART_TX : RP.GPIO.GPIO_Point := (Pin => 16);
   UART_RX : RP.GPIO.GPIO_Point := (Pin => 17);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      UART_TX.Configure (Output, Pull_Up, RP.GPIO.UART);
      UART_RX.Configure (Input, Floating, RP.GPIO.UART);
      UART.Configure
        (Config =>
           (Baud      => 31_250,
            Word_Size => 8,
            Parity    => False,
            Stop_Bits => 1,
            others    => <>));

   end Initialize;

   ------------
   -- Update --
   ------------

   procedure Update is
      G : BBqueue.Buffers.Read_Grant;
      Status  : UART_Status;
   begin
      WNM.MIDI.Queues.MIDI_Out_Read (G);
      if State (G) = Valid then
         declare
            Buffer : UART_Data_8b (1 .. Integer (Slice (G).Length))
              with Address => Slice (G).Addr;
         begin
            UART.Transmit (Buffer, Status);
            if Status /= Ok then
               raise Program_Error with "MIDI out failed";
            end if;
         end;
         WNM.MIDI.Queues.MIDI_Out_Release (G);
      end if;

   end Update;

begin
   Initialize;
end WNM.RP2040.MIDI;
