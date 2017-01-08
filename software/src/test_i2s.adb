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

with HAL.Audio;     use HAL.Audio;
with STM32.I2C;     use STM32.I2C;
with CS43L22; use CS43L22;
with Ravenscar_Time; use Ravenscar_Time;
with STM32.DMA.Interrupts;  use STM32.DMA.Interrupts;
with HAL;           use HAL;
with STM32.DMA;  use STM32.DMA;
with WNM; use WNM;
with Interfaces; use Interfaces;
with STM32.GPIO; use STM32.GPIO;
with STM32.Device; use STM32.Device;
with Ada.Real_Time; use Ada.Real_Time;
with STM32.I2S; use STM32.I2S;
with STM32.Board;
with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;
with Utils; use Utils;
with Command; use Command;
with Effects; use Effects;
with Waves; use Waves;
with BLIT; use BLIT;

package body Test_I2S is

   My_Note : Float := 440.0;
   My_Chan : WNM.Channels := WNM.Chan_A;
   My_FX_Enabled : Boolean := False;

   procedure Initialize_I2C_GPIO;
   procedure Initialize_I2S_GPIO;
   procedure Configure_I2C;
   procedure Initialize_DMA;
   procedure My_Delay (Milli : Natural);

   procedure My_Delay (Milli : Natural) is
   begin
      delay until Clock + Milliseconds (Milli);
   end My_Delay;

   function Key_To_Freq (B : WNM.Buttons) return Float is
      (case B is
          when B9  => 261.63,
          when B2  => 277.00,
          when B10 => 293.66,
          when B3  => 311.00,
          when B11 => 329.63,
          when B12 => 349.23,
          when B5  => 370.00,
          when B13 => 392.00,
          when B6  => 415.00,
          when B14 => 440.00,
          when B7  => 466.00,
          when B15 => 493.88,
          when B16 => 523.25,
          when others => 0.0);

   procedure Write_To_Stdout (Buffer : out Audio_Buffer;
                              Freq : Float);

   Last_Sample : Float := 0.0;

   procedure Write_To_Stdout (Buffer : out Audio_Buffer;
                              Freq : Float)
   is

      Trig_Going_Up : Boolean := True;

      function Next_Sample return Integer_16;
      function Update_Filter (Sample : Float) return Float;

      function Next_Sample return Integer_16 is
         Rate_F       : constant := 48_000.0;
         Amplitude_F : constant := 5000.0;
         New_Sample  : Float;
      begin
         if Freq = 0.0 then
            return 0;
         end if;

         case My_Chan is
            when WNM.Chan_A =>
               --  Sine

               New_Sample := Sin ((2.0 * Pi * Freq) *
                                  (Float (Sample_Nb) / Rate_F));
               null;
            when WNM.Chan_B =>
               --  Saw

               New_Sample := Last_Sample + (2.0 * Freq) / Rate_F;
               if New_Sample > 1.0 then
                  New_Sample := -1.0;
               end if;

            when WNM.Chan_C =>
               --  Triangle

               if Trig_Going_Up and then Last_Sample > 1.0 then
                  Trig_Going_Up := False;
               elsif not Trig_Going_Up and then Last_Sample < -1.0 then
                  Trig_Going_Up := True;
               end if;
               New_Sample := (4.0 * Freq) / Rate_F;

               if Trig_Going_Up then
                  New_Sample := Last_Sample + New_Sample;
               else
                  New_Sample := Last_Sample - New_Sample;
               end if;

            when WNM.Chan_D =>
               --  Square

               if Sin ((2.0 * Pi * Freq) *
                       (Float (Sample_Nb) / Rate_F)) > 0.0
               then
                  New_Sample := 1.0;
               else
                  New_Sample := -1.0;
               end if;
            when others =>
               return 0;
         end case;
         Last_Sample := New_Sample;
         if My_FX_Enabled then
            New_Sample := Update_Filter (New_Sample);
         end if;
         return Integer_16 (New_Sample * Amplitude_F);
      end Next_Sample;

      type Filter_Index is mod 8;
      Filter_Param : constant array (Filter_Index) of Float :=
        (0.01, 0.05, 0.12, 0.32, 0.32, 0.12, 0.05, 0.01);
      Filter_Tab : array (Filter_Index) of Float := (others => 0.0);
      Current_Index : Filter_Index := 0;

      -------------------
      -- Update_Filter --
      -------------------

      function Update_Filter (Sample : Float) return Float is
         Ret : Float := 0.0;
      begin
         Filter_Tab (Current_Index) := Sample;
         for Param_Index in Filter_Index loop
            Ret := Ret + Filter_Param (Param_Index) * Filter_Tab (Param_Index + Current_Index);
         end loop;
         Current_Index := Current_Index + 1;
         return Ret;
      end Update_Filter;
   begin
      for I in 0 .. (Buffer'Last / 2) - 1 loop
         Buffer ((I * 2) + 1) := Next_Sample;
         Buffer ((I * 2) + 2) := Buffer ((I * 2) + 1);
         Sample_Nb := Sample_Nb + 1;
      end loop;
   end Write_To_Stdout;

   -------------------------
   -- Initialize_I2C_GPIO --
   -------------------------

   procedure Initialize_I2C_GPIO is
      Points : GPIO_Points (1 .. 2);
   begin
      Points := (PB6, PB9);

      Enable_Clock (Points);

      Configure_Alternate_Function (Points, GPIO_AF_4_I2C1);

      Configure_IO (Points,
                    (Speed       => Speed_High,
                     Mode        => Mode_AF,
                     Output_Type => Open_Drain,
                     Resistors   => Floating));
      Lock (Points);
   end Initialize_I2C_GPIO;

   ------------------------
   -- Initialize_I2_GPIO --
   ------------------------

   procedure Initialize_I2S_GPIO is
      Points : constant GPIO_Points (1 .. 4) := (PC7, PC10, PC12, PA4);
   begin
      Enable_Clock (Points);

      Configure_IO (Points,
                    (Speed       => Speed_High,
                     Mode        => Mode_AF,
                     Output_Type => Push_Pull,
                     Resistors   => Floating));

      Configure_Alternate_Function (Points, GPIO_AF_6_I2S3);

      --  Lock (Points);
   end Initialize_I2S_GPIO;

   -------------------
   -- Configure_I2C --
   -------------------

   procedure Configure_I2C
   is
      I2C_Conf : I2C_Configuration;
   begin

      Enable_Clock (I2C_1);
      My_Delay (200);
      Reset (I2C_1);

      I2C_Conf.Own_Address := 16#00#;
      I2C_Conf.Addressing_Mode := Addressing_Mode_7bit;
      I2C_Conf.General_Call_Enabled := False;
      I2C_Conf.Clock_Stretching_Enabled := True;

      I2C_Conf.Clock_Speed := 100_000;

      I2C_1.Configure (I2C_Conf);
   end Configure_I2C;

   -----------------
   --  Sensor DMA --
   -----------------

   Sensor_DMA        : STM32.DMA.DMA_Controller renames DMA_1;
   Sensor_DMA_Chan   : STM32.DMA.DMA_Channel_Selector renames
     STM32.DMA.Channel_0;
   Sensor_DMA_Stream : STM32.DMA.DMA_Stream_Selector renames
     STM32.DMA.Stream_7;
   Sensor_DMA_Int    : STM32.DMA.Interrupts.DMA_Interrupt_Controller renames
     DMA1_Stream7;


   --------------------
   -- Initialize_DMA --
   --------------------

   procedure Initialize_DMA is
      Config : DMA_Stream_Configuration;
   begin
      Enable_Clock (Sensor_DMA);
      Config.Channel := Sensor_DMA_Chan;
      Config.Direction := Memory_To_Peripheral;
      Config.Increment_Peripheral_Address := False;
      Config.Increment_Memory_Address := True;
      Config.Peripheral_Data_Format := HalfWords;
      Config.Memory_Data_Format := HalfWords;
      Config.Operation_Mode := Normal_Mode;
      Config.Priority := Priority_High;
      Config.FIFO_Enabled := True;
      Config.FIFO_Threshold := FIFO_Threshold_Half_Full_Configuration;
      Config.Memory_Burst_Size := Memory_Burst_Single;
      Config.Peripheral_Burst_Size := Peripheral_Burst_Single;
      Configure (Sensor_DMA, Sensor_DMA_Stream, Config);
   end Initialize_DMA;

   Conf : I2S_Configuration;
   DAC : CS43L22_Device (I2C_1'Access,
                         Ravenscar_Time.Delays);

   Data1, Data2 : Audio_Buffer (1 .. Generator_Buffer_Length * 2);
   Status : DMA_Error_Code;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Initialize_I2C_GPIO;
      Configure_I2C;

      Set_PLLI2S_Factors (Pll_N => 258,
                          Pll_R => 3);

      Enable_PLLI2S;
      Initialize_I2S_GPIO;

      Enable_Clock (I2S_3);

      Conf.Mode                     := Master_Transmit;
      Conf.Standard                 := I2S_Philips_Standard;
      Conf.Clock_Polarity           := Steady_State_Low;
      Conf.Data_Length              := Data_16bits;
      Conf.Chan_Length              := Channel_16bits;
      Conf.Master_Clock_Out_Enabled := True;
      Conf.Transmit_DMA_Enabled     := True;
      Conf.Receive_DMA_Enabled      := False;

      I2S_3.Configure (Conf);
      I2S_3.Set_Frequency (Audio_Freq_48kHz);
      I2S_3.Enable;

      Initialize_DMA;

      Enable_Clock (PD4);

      PD4.Configure_IO ((Speed       => Speed_25MHz,
                         Mode        => Mode_Out,
                         Output_Type => Push_Pull,
                         Resistors   => Pull_Down));
      PD4.Clear;
      My_Delay (200);
      PD4.Set;
      My_Delay (200);

      DAC.Init (Output    => Headphone,
                Volume    => 50,
                Frequency => Audio_Freq_48kHz);
      DAC.Set_Volume (50);
   --  DAC.Play;

      STM32.Board.Initialize_LEDs;
   end Init;

   DMA : constant Boolean := True;

   ----------
   -- Play --
   ----------

   procedure Play is
   begin
      Data1 := (others => 0);
      Data2 := (others => 0);

      for Cnt in 1 .. 1_000_000 loop

         if DMA then

            if Cnt mod 2 = 1 then
               Sensor_DMA_Int.Start_Transfer
                 (Source      => Data2 (Data2'First)'Address,
                  Destination => I2S_3.Data_Register_Address,
                  Data_Count  => Data2'Length);
               STM32.Board.Turn_On (STM32.Board.Red);
               Write_To_Stdout (Data1, My_Note);
               STM32.Board.Turn_Off (STM32.Board.Red);
            else
               Sensor_DMA_Int.Start_Transfer
                 (Source      => Data1 (Data1'First)'Address,
                  Destination => I2S_3.Data_Register_Address,
                  Data_Count  => Data1'Length);
               STM32.Board.Turn_On (STM32.Board.Red);
               Write_To_Stdout (Data2, My_Note);
               STM32.Board.Turn_Off (STM32.Board.Red);
            end if;
            STM32.Board.Turn_On (STM32.Board.Green);
            Sensor_DMA_Int.Wait_For_Completion (Status);
            STM32.Board.Turn_Off (STM32.Board.Green);

            if Status /= DMA_No_Error then
               if Status = DMA_Timeout_Error then
                  raise Program_Error with "DMA timeout! Transferred: " &
                    Items_Transferred (Sensor_DMA, Sensor_DMA_Stream)'Img;
               elsif Items_Transferred (Sensor_DMA, Sensor_DMA_Stream) /= 0 then
                  raise Program_Error with "DMA Error! Transferred: " &
                    Items_Transferred (Sensor_DMA, Sensor_DMA_Stream)'Img;
               end if;
            end if;
         else
            I2S_3.Transmit (Data1);
         end if;
      end loop;
   end Play;

   --------------
   -- Play_ASL --
   --------------

   procedure Play_ASL is
      pragma Suppress (Accessibility_Check);
      BPM : constant := 120;
      SNL : constant Sample_Period := 4000;

      S1 : constant Sequencer_Note := ((C, 4), SNL);
      S2 : constant Sequencer_Note := ((F, 4), SNL);
      S3 : constant Sequencer_Note := ((D_Sh, 4), SNL);
      S4 : constant Sequencer_Note := ((A_Sh, 4), SNL);
      S5 : constant Sequencer_Note := ((G, 4), SNL);
      S6 : constant Sequencer_Note := ((D_Sh, 4), SNL);

      Synth_Seq : constant access Simple_Sequencer :=
        Create_Sequencer
          (8, BPM, 4,
           Notes =>
             (S1, S1, S1, S1, S1, S2, S2, S2,
              S3, S3, S3, S3, S3, S4, S4, S4,
              S1, S1, S1, S1, S1, S2, S2, S2,
              S5, S5, S5, S5, S5, S6, S6, S6));

      Synth_Source : constant Note_Generator_Access :=
        Note_Generator_Access (Synth_Seq);

      Synth : constant access Disto :=
      --  We distort the output signal of the synthetizer with a soft clipper
        Create_Dist
          (Clip_Level => 1.00001,
           Coeff      => 1.5,

           --  The oscillators of the synth are fed to an LP filter
           Source     => Create_LP
             (

                  --  We use an ADSR enveloppe to modulate the Cut frequency of the
              --  filter. Using it as the modulator of a Fixed generator allows us
              --  to have a cut frequency that varies between 1700 hz and 200 hz.
              Cut_Freq =>
                Fixed
                  (Freq      => 200.0,
                   Modulator => new Attenuator'
                     (Level  => 1500.0,
                      Source => Create_ADSR (10, 150, 200, 0.005, Synth_Source),
                      others => <>)),

              --  Q is the resonance of the filter, very high values will give a
              --  resonant sound.
              Q => 0.2,

              --  This is the mixer, receiving the sound of 4 differently tuned
              --  oscillators, 1 sine and 3 saws
              Source =>
                Create_Mixer
                  (Sources =>
                       (4 => (Create_Sine
                              (Create_Pitch_Gen
                                 (Rel_Pitch => -30, Source => Synth_Source)),
                              Level => 0.6),
                        3 => (BLIT.Create_Saw
                              (Create_Pitch_Gen
                                 (Rel_Pitch => -24, Source => Synth_Source)),
                              Level => 0.3),
                        2 => (BLIT.Create_Saw
                              (Create_Pitch_Gen
                                 (Rel_Pitch => -12, Source => Synth_Source)),
                              Level => 0.3),
                        1 => (BLIT.Create_Saw
                              (Create_Pitch_Gen
                                 (Rel_Pitch => -17, Source => Synth_Source)),
                              Level => 0.5)))));

      Main_Mixer : constant access Mixer :=
        Create_Mixer ((1 => (Synth, 0.001)));

      procedure Audio (Buffer : out Audio_Buffer);
      procedure Audio (Buffer : out Audio_Buffer) is

         function Sample_To_Uint16 is new Sample_To_Int (Integer_16);
      begin

         Next_Steps;
         Main_Mixer.Next_Samples;

         for I in B_Range_T loop
            --  Buffer (Integer (I)) := Sample_To_Uint16 (Test.Buffer (I));
            Buffer (Integer (I * 2) + 1) := Sample_To_Uint16 (Main_Mixer.Buffer (I * 2));
            Buffer (Integer (I * 2) + 2) := Buffer (Integer (I * 2) + 1);
         end loop;
      end Audio;
   begin
      Data1 := (others => 0);
      Data2 := (others => 0);

      for Cnt in 1 .. 1_000_000 loop

         if DMA then

            if Cnt mod 2 = 1 then
               Sensor_DMA_Int.Start_Transfer
                 (Source      => Data2 (Data2'First)'Address,
                  Destination => I2S_3.Data_Register_Address,
                  Data_Count  => Data2'Length);
               STM32.Board.Turn_On (STM32.Board.Red);
               Audio (Data1);
               STM32.Board.Turn_Off (STM32.Board.Red);
            else
               Sensor_DMA_Int.Start_Transfer
                 (Source      => Data1 (Data1'First)'Address,
                  Destination => I2S_3.Data_Register_Address,
                  Data_Count  => Data1'Length);
               STM32.Board.Turn_On (STM32.Board.Red);
               Audio (Data2);
               STM32.Board.Turn_Off (STM32.Board.Red);
            end if;
            STM32.Board.Turn_On (STM32.Board.Green);
            Sensor_DMA_Int.Wait_For_Completion (Status);
            STM32.Board.Turn_Off (STM32.Board.Green);

            if Status /= DMA_No_Error then
               if Status = DMA_Timeout_Error then
                  raise Program_Error with "DMA timeout! Transferred: " &
                    Items_Transferred (Sensor_DMA, Sensor_DMA_Stream)'Img;
               elsif Items_Transferred (Sensor_DMA, Sensor_DMA_Stream) /= 0 then
                  raise Program_Error with "DMA Error! Transferred: " &
                    Items_Transferred (Sensor_DMA, Sensor_DMA_Stream)'Img;
               end if;
            end if;
         else
            I2S_3.Transmit (Data1);
         end if;
      end loop;
   end Play_ASL;

   ----------------------
   -- Set_Current_Note --
   ----------------------

   procedure Set_Current_Note (B : Buttons) is
   begin
      My_Note := Key_To_Freq (B);
   end Set_Current_Note;

   -------------------------
   -- Set_Current_Channel --
   -------------------------

   procedure Set_Current_Channel (Chan : WNM.Channels) is
   begin
      My_Chan := Chan;
   end Set_Current_Channel;


   ---------------
   -- Enable_FX --
   ---------------

   procedure Enable_FX is
   begin
      My_FX_Enabled := True;
   end Enable_FX;

   ----------------
   -- Disable_FX --
   ----------------

   procedure Disable_FX is
   begin
      My_FX_Enabled := False;
   end Disable_FX;

end Test_I2S;
