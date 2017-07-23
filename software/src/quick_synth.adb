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

with HAL.Audio;            use HAL.Audio;
with Interfaces;           use Interfaces;
with Simple_Synthesizer;
with HAL; use HAL;

package body Quick_Synth is

   use type WNM.Tracks;

   My_Synths : array (WNM.Tracks) of  Simple_Synthesizer.Synthesizer
     (Stereo    => True,
      Amplitude => Natural (Unsigned_16'Last / 10));

   Track_Muted : array (WNM.Tracks) of Boolean := (others => False);
   Solo_Mode_Enabled : Boolean := False;
   Solo_Track : WNM.Tracks := WNM.Track_A;

   -----------
   -- Event --
   -----------

   procedure Event (Msg : MIDI.Message) is
      Track : constant WNM.Tracks := WNM.To_Track (Msg.Channel);
   begin

      case Msg.Kind is
         when MIDI.Note_On =>
            My_Synths (Track).Set_Note_Frequency
              (MIDI.Key_To_Frequency (Msg.Cmd.Key));
         when  MIDI.Note_Off =>
            if My_Synths (Track).Note_Frequency = MIDI.Key_To_Frequency (Msg.Cmd.Key) then
               My_Synths (Track).Set_Note_Frequency (0.0);
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

      for Track in WNM.Tracks loop
         My_Synths (Track).Receive (Tmp);

         if (not Track_Muted (Track) and then not Solo_Mode_Enabled)
           or else
            (not Track_Muted (Track) and then Solo_Track = Track)
           or else
            (Solo_Mode_Enabled and then Solo_Track = Track)
         then
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

   procedure Mute (Track : WNM.Tracks) is
   begin
      Track_Muted (Track) := True;
   end Mute;

   ------------
   -- Unmute --
   ------------

   procedure Unmute (Track : WNM.Tracks) is
   begin
      Track_Muted (Track) := False;
   end Unmute;

   -----------------
   -- Toggle_Mute --
   -----------------

   procedure Toggle_Mute (Track : WNM.Tracks) is
   begin
      Track_Muted (Track) := not Track_Muted (Track);
   end Toggle_Mute;

   -----------
   -- Muted --
   -----------

   function Muted (Track : WNM.Tracks) return Boolean
   is (Track_Muted (Track));

   -----------------
   -- Toggle_Solo --
   -----------------

   procedure Toggle_Solo (Track : WNM.Tracks) is
   begin
      if Solo_Mode_Enabled then
         if Solo_Track = Track then
            Solo_Mode_Enabled := False;
         else
            Solo_Track := Track;
         end if;
      else
         Solo_Mode_Enabled := True;
         Solo_Track := Track;
      end if;
   end Toggle_Solo;

   -------------
   -- In_Solo --
   -------------

   function In_Solo return Boolean
   is (Solo_Mode_Enabled);

   ----------
   -- Solo --
   ----------

   function Solo return WNM.Tracks
   is (Solo_Track);

begin
   for Synth of My_Synths loop
      Synth.Set_Frequency (Audio_Freq_48kHz);
      Synth.Set_Note_Frequency (0.0);
   end loop;
end Quick_Synth;
