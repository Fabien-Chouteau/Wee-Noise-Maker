with HAL.GPIO;
with HAL.UART;

with SAM.SERCOM.USART;
with SAM.Port;
with SAM.Clock_Generator.IDs;
with SAM.Functions;
with SAM.Main_Clock;

with WNM.MIDI;
with WNM.MIDI.Queues;

package body WNM.Samd51.MIDI is

   UART : SAM.SERCOM.USART.USART_Device renames SAM.Device.USART5;
   RX   : SAM.Port.GPIO_Point renames SAM.Device.PB17;
   TX   : SAM.Port.GPIO_Point renames SAM.Device.PB16;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin

      -- IOs --

      RX.Clear;
      RX.Set_Mode (HAL.GPIO.Output);
      RX.Set_Pull_Resistor (HAL.GPIO.Floating);
      RX.Set_Function (SAM.Functions.PB17_SERCOM5_PAD1);

      TX.Clear;
      TX.Set_Mode (HAL.GPIO.Output);
      TX.Set_Pull_Resistor (HAL.GPIO.Floating);
      TX.Set_Function (SAM.Functions.PB16_SERCOM5_PAD0);

      -- Clocks --
      SAM.Clock_Generator.Configure_Periph_Channel
        (SAM.Clock_Generator.IDs.SERCOM5_CORE,
         SAM.Clock_Setup_120Mhz.Clk_48Mhz);

      SAM.Clock_Generator.Configure_Periph_Channel
        (SAM.Clock_Generator.IDs.SERCOM5_SLOW,
         SAM.Clock_Setup_120Mhz.Clk_32Khz);

      -- USART --

      SAM.Main_Clock.SERCOM5_On;

      UART.Configure (Baud            => 64_855, -- MIDI should be 31,250 bits/s
                      MSB_First       => False,
                      TX_Fall_RX_Rise => False,
                      Parity          => False,
                      Synchronous_Com => False,
                      RXPO            => 1,
                      TXPO            => 2,
                      Run_In_Standby  => False);

      UART.Debug_Stop_Mode (Enabled => True);

      UART.Enable;
      UART.Enable_Transmitter;

   --     Loop
   --        declare
   --           Status : HAL.UART.UART_Status;
   --        begin
   --           UART.Transmit (Data    => HAL.UART.UART_Data_8b'(0 => 16#90#,
   --                                                            1 => 16#2A#,
   --                                                            2 => 16#40#),
   --                          Status  => Status);
   --           WNM.Time.Delay_Ms (300);
   --        end;
   --     end loop;
   end Initialize;

   ------------
   -- Update --
   ------------

   function Update return Time.Time_Ms is
      procedure Process (Msg : WNM.MIDI.Message) is
         Status : HAL.UART.UART_Status;
         Data    : HAL.UART.UART_Data_8b (0 .. 2) with Address => Msg'Address;
      begin
         UART.Transmit (Data, Status);
      end Process;

      procedure Pop is new  WNM.MIDI.Queues.MIDI_Out_Pop (Process);
   begin
      Pop;
      return Time.Time_Ms'First;
   end Update;

begin
   Initialize;
end WNM.Samd51.MIDI;
