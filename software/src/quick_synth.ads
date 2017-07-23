with HAL.Audio;
with MIDI;
with WNM;

package Quick_Synth is
   procedure Event (Msg : MIDI.Message);
   procedure Fill (Input  :     HAL.Audio.Audio_Buffer;
                   Output : out HAL.Audio.Audio_Buffer);

   procedure Mute (Chan : WNM.Channels);
   procedure Unmute (Chan : WNM.Channels);
   procedure Toggle_Mute (Chan : WNM.Channels);
   function Muted (Chan : WNM.Channels) return Boolean;

   procedure Toggle_Solo (Chan : WNM.Channels);
   function In_Solo return Boolean;
   function Solo return WNM.Channels;

end Quick_Synth;
