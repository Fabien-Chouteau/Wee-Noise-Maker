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

with HAL.Audio;                  use HAL.Audio;
with HAL;                        use HAL;
with WNM.Sample_Stream;          use WNM.Sample_Stream;
with Managed_Buffers;            use Managed_Buffers;
with WNM;                        use WNM;
with WNM.Buffer_Allocation;      use WNM.Buffer_Allocation;
with WNM.Sample_Library;         use WNM.Sample_Library;
with Hex_Dump;
with Semihosting;

package body Quick_Synth is

   Track_Muted : array (WNM.Tracks) of Boolean := (others => False);
   Solo_Mode_Enabled : Boolean := False;
   Solo_Track : WNM.Tracks := B1;

   Pan_For_Track : array (WNM.Tracks) of Integer := (others => 0)
     with Atomic_Components;
   Volume_For_Track : array (WNM.Tracks) of Integer := (others => 50)
     with Atomic_Components;
   Sample_For_Track : array (WNM.Tracks) of Sample_Entry_Index :=
     (others => Invalid_Sample_Entry)
     with Atomic_Components;

   procedure Copy (Src : not null Any_Managed_Buffer;
                   Dst : out Mono_Buffer);
   procedure Copy_Stereo_To_Mono (Src : Stereo_Buffer;
                                  Dst : not null Any_Managed_Buffer);
   function Is_It_On (Track : Stream_Track) return Boolean;
   procedure Audio_Hex_Dump (Info : String;
                             Buffer : HAL.Audio.Audio_Buffer);
   pragma Unreferenced (Audio_Hex_Dump);

   --------------------
   -- Audio_Hex_Dump --
   --------------------

   procedure Audio_Hex_Dump (Info : String;
                             Buffer : HAL.Audio.Audio_Buffer)
   is
      Data : HAL.UInt8_Array (1 .. Buffer'Length / 2)
        with Address => Buffer'Address;
   begin
      Semihosting.Log_Line (Info);
      Hex_Dump.Hex_Dump (Data,
                         Put_Line  => Semihosting.Log_Line'Access,
                         Base_Addr => 0);
   end Audio_Hex_Dump;

   ----------
   -- Copy --
   ----------

   procedure Copy (Src : not null Any_Managed_Buffer;
                   Dst : out Mono_Buffer)
   is
      Data : Mono_Buffer with Address => Src.Buffer_Address;
   begin

      if Src.Buffer_Length /= Mono_Buffer_Size_In_Bytes then
         raise Program_Error with "WTF!?!";
      end if;

      Dst := Data;
   end Copy;

   -------------------------
   -- Copy_Stereo_To_Mono --
   -------------------------

   procedure Copy_Stereo_To_Mono (Src : Stereo_Buffer;
                                  Dst : not null Any_Managed_Buffer)
   is
      Data : Mono_Buffer with Address => Dst.Buffer_Address;
      Tmp  : Integer_32;
   begin

      if Dst.Buffer_Length /= Mono_Buffer_Size_In_Bytes then
         raise Program_Error with "WTF!?!";
      end if;

      for Index in Data'Range loop
         Tmp := Integer_32 (Src (Index).L) + Integer_32 (Src (Index).R);
         Tmp := Tmp / 2;

         if Tmp > Integer_32 (Mono_Sample'Last) then
            Data (Index) := Mono_Sample'Last;
         elsif Tmp < Integer_32 (Integer_16'First) then
            Data (Index) := Mono_Sample'First;
         else
            Data (Index) := Mono_Sample (Tmp);
         end if;
      end loop;
   end Copy_Stereo_To_Mono;

   --------------
   -- Is_It_On --
   --------------

   function Is_It_On (Track : Stream_Track) return Boolean
   is (Track = Always_On
       or else
         (not Track_Muted (To_Track (Track)) and then not Solo_Mode_Enabled)
       or else
         (not Track_Muted (To_Track (Track)) and then Solo_Track = To_Track (Track))
       or else
         (Solo_Mode_Enabled and then Solo_Track = To_Track (Track)));

   -----------
   -- Event --
   -----------

   procedure Event (Msg : MIDI.Message) is
      Track : constant WNM.Tracks := WNM.To_Track (Msg.Channel);
   begin

      case Msg.Kind is
         when MIDI.Note_On =>
            if Track = B1 then
               Start (Filepath    => (case Msg.Cmd.Key is
                                         when MIDI.C4  => "/sdcard/drums/Clap.raw",
                                         when MIDI.Cs4 => "/sdcard/drums/Clave.raw",
                                         when MIDI.D4  => "/sdcard/drums/Cymbal-high.raw",
                                         when MIDI.Ds4 => "/sdcard/drums/Hat_closed.raw",
                                         when MIDI.E4  => "/sdcard/drums/Hat_long.raw",
                                         when MIDI.F4  => "/sdcard/drums/Hi_Tom.raw",
                                         when MIDI.Fs4 => "/sdcard/drums/Kick_long.raw",
                                         when MIDI.G4  => "/sdcard/drums/Kick_short.raw",
                                         when MIDI.Gs4 => "/sdcard/drums/Lo_Tom.raw",
                                         when MIDI.A4  => "/sdcard/drums/Md_Tom.raw",
                                         when MIDI.As4 => "/sdcard/drums/Rim_Shot.raw",
                                         when MIDI.B4  => "/sdcard/drums/Snare_lo1.raw",
                                         when MIDI.C5  => "/sdcard/drums/Snare_lo2.raw",
                                         when MIDI.Cs5 => "/sdcard/drums/Snare_lo3.raw",
                                         when MIDI.D5  => "/sdcard/drums/Cowbell.raw",
                                         when MIDI.Ds5 => "/sdcard/drums/Maracas.raw",
                                         when MIDI.E5  => "/sdcard/drums/Hi_Conga.raw",
                                         when MIDI.F5  => "/sdcard/drums/Md_Conga.raw",
                                         when MIDI.Fs5 => "/sdcard/quotes/wake.raw",
                                         when MIDI.G5  => "/sdcard/quotes/darkside.raw",
                                         when MIDI.Gs5 => "/sdcard/quotes/failure2_x.raw",
                                         when MIDI.A5  => "/sdcard/quotes/failure3.raw",
                                         when MIDI.As5 => "/sdcard/quotes/halbye.raw",
                                         when MIDI.B5  => "/sdcard/quotes/learn.raw",
                                         when MIDI.C6  => "/sdcard/quotes/trap.raw",
                                         when others   => "/sdcard/test.raw"),
                      Start_Point => 0,
                      End_Point   => Natural'Last,
                      Track       => To_Stream_Track (Track),
                      Looping     => False,
                      Poly        => True);
            end if;
         when  MIDI.Note_Off =>
            null;
         when others =>
            null;
      end case;
   end Event;

   ----------
   -- Trig --
   ----------

   procedure Trig (Track : WNM.Tracks) is
   begin
      if Sample_For_Track (Track) /= Invalid_Sample_Entry then
         Start (Filepath    => Entry_Path (Sample_For_Track (Track)),
                Start_Point => 0,
                End_Point   => Natural'Last,
                Track       => To_Stream_Track (Track),
                Looping     => False,
                Poly        => True);
      end if;
   end Trig;

   ------------------
   -- Load_Samples --
   ------------------

   procedure Load_Samples is
   begin
      for Track in Tracks loop
         Assign_Sample (Track,
                        Entry_From_Path ((case Track is
                             when B1 => "/sdcard/samples/drums/clap/Clap.raw",
                             when B6 => "/sdcard/samples/drums/hat/Hat_closed.raw",
                             when B2 => "/sdcard/samples/drums/kick/Kick_short.raw",
                             when B3 => "/sdcard/samples/drums/misc/Rim_Shot.raw",
                             when B4 => "/sdcard/samples/drums/snare/Snare_lo1.raw",
                             when B5 => "/sdcard/samples/drums/snare/Snare_lo2.raw",
                             when B7 => "/sdcard/samples/misc/Cowbell.raw",
                             when B8 => "/sdcard/samples/drums/tom/Md_Tom.raw",
                             when B9  => "/sdcard/samples/vocals/the_way_news_goes.raw",
                             when B10 => "/sdcard/samples/vocals/show_me_what_you_got.raw",
                             when B11 => "/sdcard/samples/vocals/pickle_rick.raw",
                             when B12 => "/sdcard/samples/vocals/wake.raw",
                             when B13 => "/sdcard/samples/vocals/darkside.raw",
                             when B14 => "/sdcard/samples/vocals/failure2_x.raw",
                             when B15 => "/sdcard/samples/vocals/halbye.raw",
                             when B16 => "/sdcard/samples/user/AAA.raw")));
      end loop;
   end Load_Samples;

   -------------------
   -- Assign_Sample --
   -------------------

   procedure Assign_Sample (Track  : WNM.Tracks;
                            Sample : WNM.Sample_Library.Sample_Entry_Index)
   is
   begin
      Sample_For_Track (Track) := Sample;
   end Assign_Sample;

   ---------------------
   -- Sample_Of_Track --
   ---------------------

   function Sample_Of_Track (Track : WNM.Tracks)
                             return WNM.Sample_Library.Sample_Entry_Index
   is
   begin
      return Sample_For_Track (Track);
   end Sample_Of_Track;

   ----------
   -- Fill --
   ----------

   procedure Fill (Stereo_Input  :     Stereo_Buffer;
                   Stereo_Output : out Stereo_Buffer)
   is

      procedure Mix (Mono_Samples : Mono_Buffer;
                     ST           : Stream_Track);

      ---------
      -- Mix --
      ---------

      procedure Mix (Mono_Samples : Mono_Buffer;
                     ST           : Stream_Track)
      is
         Val         : Integer_32;

         Sample, Left, Right : Float;

         Volume       : Float;
         Pan          : Float;
      begin
         if ST = Always_On then
            Volume := 1.0;
            Pan    := 0.0;
         else
            Volume := Float (Volume_For_Track (To_Track (ST))) / 50.0;

            --  FIXME: Hack to fix the panning problem
            Pan    := Float (Pan_For_Track (To_Track (ST)) + 100) / 200.0;
            --  Pan    := Float (Pan_For_Track (To_Track (ST))) / 100.0;
         end if;

--           Audio_Hex_Dump ("Stereo_Output before mix:", Stereo_Output);

         for Index in Mono_Samples'Range loop

            Sample := Float (Mono_Samples (Index));
            Sample := Sample * Volume;

            Right := Sample * (1.0 - Pan);
            Left  := Sample * (1.0 + Pan);

            Val := Integer_32 (Stereo_Output (Index).L) + Integer_32 (Left);
            if Val > Integer_32 (Mono_Sample'Last) then
               Stereo_Output (Index).L := Mono_Sample'Last;
            elsif Val < Integer_32 (Integer_16'First) then
               Stereo_Output (Index).L := Mono_Sample'First;
            else
               Stereo_Output (Index).L := Mono_Sample (Val);
            end if;

            Val := Integer_32 (Stereo_Output (Index).R) + Integer_32 (Right);
            if Val > Integer_32 (Mono_Sample'Last) then
               Stereo_Output (Index).R := Mono_Sample'Last;
            elsif Val < Integer_32 (Integer_16'First) then
               Stereo_Output (Index).R := Mono_Sample'First;
            else
               Stereo_Output (Index).R := Mono_Sample (Val);
            end if;
         end loop;
--           Audio_Hex_Dump ("Stereo_Output after mix:", Stereo_Output);
      end Mix;

      Mono_Tmp : Mono_Buffer;
      Buf      : Any_Managed_Buffer;
      On_Track : Stream_Track;
   begin

      -- Audio in --
      Stereo_Output := Stereo_Input;

      -- Samples streams --

      for ID in Valid_Stream_ID loop
         Next_Buffer (ID, Buf, On_Track);
         if Buf /= null then
            if Is_It_On (On_Track) then
               Copy (Buf, Mono_Tmp);
               Mix (Mono_Tmp, On_Track);
            end if;
            Release_Buffer (Buf);
         end if;
      end loop;

      -- Recording --
      case WNM.Sample_Stream.Now_Recording is
         when WNM.Sample_Stream.None =>
            null;
         when WNM.Sample_Stream.Input =>
            declare
               Buffer : constant Any_Managed_Buffer :=
                 Allocate (Kind => RAM,
                           Size => Mono_Buffer_Size_In_Bytes);
            begin
               if Buffer = null then
                  raise Program_Error with "Cannot allocate buffer...";
               end if;

               Copy_Stereo_To_Mono (Stereo_Input, Buffer);
               WNM.Sample_Stream.Push_Record_Buffer (Buffer);
            end;
         when WNM.Sample_Stream.Master_Output =>
            declare
               Buffer : constant Any_Managed_Buffer :=
                 Allocate (Kind => RAM,
                           Size => Mono_Buffer_Size_In_Bytes);
            begin
               if Buffer = null then
                  raise Program_Error with "Cannot allocate buffer...";
               end if;

               Copy_Stereo_To_Mono (Stereo_Output, Buffer);
               WNM.Sample_Stream.Push_Record_Buffer (Buffer);
            end;
      end case;
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

   ----------------
   -- Change_Pan --
   ----------------

   procedure Change_Pan (Track : WNM.Tracks;
                         Pan   : Integer)
   is
   begin
      Pan_For_Track (Track) := Pan_For_Track (Track) + Pan;
      if Pan_For_Track (Track) > 100 then
         Pan_For_Track (Track) := 100;
      elsif Pan_For_Track (Track) < -100 then
         Pan_For_Track (Track) := -100;
      end if;
   end Change_Pan;

   ---------
   -- Pan --
   ---------

   function Pan (Track : WNM.Tracks) return Integer
   is (Pan_For_Track (Track));

   -------------------
   -- Change_Volume --
   -------------------

   procedure Change_Volume (Track  : WNM.Tracks;
                            Volume : Integer)
   is
   begin
      Volume_For_Track (Track) := Volume_For_Track (Track) + Volume;
      if Volume_For_Track (Track) > 100 then
         Volume_For_Track (Track) := 100;
      elsif Volume_For_Track (Track) < 0 then
         Volume_For_Track (Track) := 0;
      end if;
   end Change_Volume;

   ------------
   -- Volume --
   ------------

   function Volume (Track : WNM.Tracks) return Natural
   is (Natural (Volume_For_Track (Track)));

end Quick_Synth;
