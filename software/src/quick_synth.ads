with HAL.Audio;
with MIDI;

package Quick_Synth is
   procedure Event (Msg : MIDI.Message);
   procedure Fill (Input  :     HAL.Audio.Audio_Buffer;
                   Output : out HAL.Audio.Audio_Buffer);
end Quick_Synth;
