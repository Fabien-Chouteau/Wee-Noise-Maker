with HAL.Audio;
with MIDI;
with WNM;

package Quick_Synth is
   procedure Event (Msg : MIDI.Message);
   procedure Fill (Input  :     HAL.Audio.Audio_Buffer;
                   Output : out HAL.Audio.Audio_Buffer);

   procedure Mute (Track : WNM.Tracks);
   procedure Unmute (Track : WNM.Tracks);
   procedure Toggle_Mute (Track : WNM.Tracks);
   function Muted (Track : WNM.Tracks) return Boolean;

   procedure Toggle_Solo (Track : WNM.Tracks);
   function In_Solo return Boolean;
   function Solo return WNM.Tracks;

end Quick_Synth;
