with HAL.Audio;            use HAL.Audio;
with Interfaces;           use Interfaces;
with Simple_Synthesizer;
with HAL; use HAL;

package body Quick_Synth is

   My_Synths : array (WNM.Channels) of  Simple_Synthesizer.Synthesizer
     (Stereo    => True,
      Amplitude => Natural (Unsigned_16'Last / 10));

   Chan_Muted : array (WNM.Channels) of Boolean := (others => False);

   -----------
   -- Event --
   -----------

   procedure Event (Msg : MIDI.Message) is
      use type WNM.Channels;
      Chan : constant WNM.Channels := WNM.To_Channel (Msg.Channel);
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

      for Chan in WNM.Channels loop
         My_Synths (Chan).Receive (Tmp);

         if not Chan_Muted (Chan) then
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
         end if;
      end loop;
   end Fill;

   ----------
   -- Mute --
   ----------

   procedure Mute (Chan : WNM.Channels) is
   begin
      Chan_Muted (Chan) := True;
   end Mute;

   ------------
   -- Unmute --
   ------------

   procedure Unmute (Chan : WNM.Channels) is
   begin
      Chan_Muted (Chan) := False;
   end Unmute;

   -----------------
   -- Toggle_Mute --
   -----------------

   procedure Toggle_Mute (Chan : WNM.Channels) is
   begin
      Chan_Muted (Chan) := not Chan_Muted (Chan);
   end Toggle_Mute;

   -----------
   -- Muted --
   -----------

   function Muted (Chan : WNM.Channels) return Boolean
   is (Chan_Muted (Chan));

begin
   for Synth of My_Synths loop
      Synth.Set_Frequency (Audio_Freq_48kHz);
      Synth.Set_Note_Frequency (0.0);
   end loop;
end Quick_Synth;
