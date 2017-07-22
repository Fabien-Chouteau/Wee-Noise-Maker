with HAL.Audio;            use HAL.Audio;
with Interfaces;           use Interfaces;
with Simple_Synthesizer;
with HAL; use HAL;

with WNM;

package body Quick_Synth is

   My_Synths : array (WNM.Channels) of  Simple_Synthesizer.Synthesizer
     (Stereo    => True,
      Amplitude => Natural (Unsigned_16'Last / 10));

   -----------
   -- Event --
   -----------

   procedure Event (Msg : MIDI.Message) is
      use type WNM.Channels;

      Chan : constant WNM.Channels := (case Msg.Channel is
                                          when 0 => WNM.Chan_A,
                                          when 1 => WNM.Chan_B,
                                          when 2 => WNM.Chan_C,
                                          when 3 => WNM.Chan_D,
                                          when 4 => WNM.Chan_E,
                                          when others => WNM.Chan_A);
   begin

      case Msg.Kind is
         when MIDI.Note_On =>
            My_Synths (Chan).Set_Note_Frequency
              (MIDI.Key_To_Frequency (Msg.Cmd.Key));
         when  MIDI.Note_Off =>
            if My_Synths (Chan).Note_Frequency = MIDI.Key_To_Frequency (Msg.Cmd.Key) then
               My_Synths (Chan).Set_Note_Frequency (0.0);
            end if;
         when others =>
            null;
      end case;
   end Event;


   ----------
   -- Fill --
   ----------

   procedure Fill (Input  :     HAL.Audio.Audio_Buffer;
                   Output : out HAL.Audio.Audio_Buffer)
   is
      Tmp : HAL.Audio.Audio_Buffer (Output'Range);
      Val : Integer_32;
   begin

      for Index in Output'Range loop
         Output (Index) := Input (Index);
      end loop;

      for Synth of My_Synths loop
         Synth.Receive (Tmp);
         for Index in Output'Range loop
            Val := Integer_32 (Output (Index)) + Integer_32 (Tmp (Index));
            if Val > Integer_32 (Integer_16'Last) then
               Output (Index) := Integer_16'Last;
            elsif Val < Integer_32 (Integer_16'First) then
               Output (Index) := Integer_16'First;
            else
               Output (Index) := Integer_16 (Val);
            end if;
         end loop;
      end loop;
   end Fill;
begin
   for Synth of My_Synths loop
      Synth.Set_Frequency (Audio_Freq_48kHz);
      Synth.Set_Note_Frequency (0.0);
   end loop;
end Quick_Synth;
