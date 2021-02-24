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

with HAL; use HAL;
with Interfaces; use Interfaces;
with WNM.Sample_Stream;          use WNM.Sample_Stream;
with WNM;                        use WNM;
--  with WNM.Sample_Library;         use WNM.Sample_Library;
with WNM.Audio;                  use WNM.Audio;
with WNM.File_System;            use WNM.File_System;

--  with Hex_Dump;
--  with Semihosting;

package body WNM.Synth is

   Recording_File   : aliased File_Descriptor;
   Recording_Source : Rec_Source;
   Recording_Size   : File_Signed_Size;

   Track_Muted : array (WNM.Tracks) of Boolean := (others => False);
   Solo_Mode_Enabled : Boolean := False;
   Solo_Track : WNM.Tracks := B1;

   Pan_For_Track : array (WNM.Tracks) of Integer := (others => 0)
     with Atomic_Components;
   Volume_For_Track : array (WNM.Tracks) of Integer := (others => 50)
     with Atomic_Components;
   --  Sample_For_Track : array (WNM.Tracks) of Sample_Entry_Index :=
   --    (others => Invalid_Sample_Entry)
   --    with Atomic_Components;

   Passthrough : Input_Kind := None;

   Next_Start : WNM.Time.Time_Ms := WNM.Time.Time_Ms'First;
   Glob_Sample_Clock : Sample_Time := 0 with Volatile;

   procedure Copy_Stereo_To_Mono (L, R : Mono_Buffer;
                                  Dst : out Mono_Buffer);

   function Is_It_On (Track : Stream_Track) return Boolean;

   ------------------
   -- Sample_Clock --
   ------------------

   function Sample_Clock return Sample_Time
   is (Glob_Sample_Clock);

   -------------------------
   -- Copy_Stereo_To_Mono --
   -------------------------

   procedure Copy_Stereo_To_Mono (L, R : Mono_Buffer;
                                  Dst : out Mono_Buffer)
   is
      Tmp  : Integer_32;
   begin

      for Index in Dst'Range loop
         Tmp := Integer_32 (L (Index)) + Integer_32 (R (Index));
         Tmp := Tmp / 2;

         if Tmp > Integer_32 (Mono_Sample'Last) then
            Dst (Index) := Mono_Sample'Last;
         elsif Tmp < Integer_32 (Integer_16'First) then
            Dst (Index) := Mono_Sample'First;
         else
            Dst (Index) := Mono_Sample (Tmp);
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
      --  Track : constant WNM.Tracks := WNM.To_Track (Msg.Channel);
   begin
      null;
      --  case Msg.Kind is
      --     when MIDI.Note_On =>
      --        if Track = B1 then
      --           Start (Filepath    => (case Msg.Cmd.Key is
      --                                     when MIDI.C4  => "/sdcard/drums/Clap.raw",
      --                                     when MIDI.Cs4 => "/sdcard/drums/Clave.raw",
      --                                     when MIDI.D4  => "/sdcard/drums/Cymbal-high.raw",
      --                                     when MIDI.Ds4 => "/sdcard/drums/Hat_closed.raw",
      --                                     when MIDI.E4  => "/sdcard/drums/Hat_long.raw",
      --                                     when MIDI.F4  => "/sdcard/drums/Hi_Tom.raw",
      --                                     when MIDI.Fs4 => "/sdcard/drums/Kick_long.raw",
      --                                     when MIDI.G4  => "/sdcard/drums/Kick_short.raw",
      --                                     when MIDI.Gs4 => "/sdcard/drums/Lo_Tom.raw",
      --                                     when MIDI.A4  => "/sdcard/drums/Md_Tom.raw",
      --                                     when MIDI.As4 => "/sdcard/drums/Rim_Shot.raw",
      --                                     when MIDI.B4  => "/sdcard/drums/Snare_lo1.raw",
      --                                     when MIDI.C5  => "/sdcard/drums/Snare_lo2.raw",
      --                                     when MIDI.Cs5 => "/sdcard/drums/Snare_lo3.raw",
      --                                     when MIDI.D5  => "/sdcard/drums/Cowbell.raw",
      --                                     when MIDI.Ds5 => "/sdcard/drums/Maracas.raw",
      --                                     when MIDI.E5  => "/sdcard/drums/Hi_Conga.raw",
      --                                     when MIDI.F5  => "/sdcard/drums/Md_Conga.raw",
      --                                     when MIDI.Fs5 => "/sdcard/quotes/wake.raw",
      --                                     when MIDI.G5  => "/sdcard/quotes/darkside.raw",
      --                                     when MIDI.Gs5 => "/sdcard/quotes/failure2_x.raw",
      --                                     when MIDI.A5  => "/sdcard/quotes/failure3.raw",
      --                                     when MIDI.As5 => "/sdcard/quotes/halbye.raw",
      --                                     when MIDI.B5  => "/sdcard/quotes/learn.raw",
      --                                     when MIDI.C6  => "/sdcard/quotes/trap.raw",
      --                                     when others   => "/sdcard/test.raw"),
      --                  Start_Point => 0,
      --                  End_Point   => Natural'Last,
      --                  Track       => To_Stream_Track (Track),
      --                  Looping     => False);
      --        end if;
      --     when  MIDI.Note_Off =>
      --        null;
      --     when others =>
      --        null;
      --  end case;
   end Event;

   ----------
   -- Trig --
   ----------

   procedure Trig (Track : WNM.Tracks) is
   begin
      Start (Track       => To_Stream_Track (Track),
             Start_Point => 0,
             End_Point   => Natural'Last,
             Looping     => False);
   end Trig;

   ------------------
   -- Load_Samples --
   ------------------

   procedure Load_Samples is
   begin
      --  Assign_Sample (ST_1, "/samples/kick");
      --  Assign_Sample (ST_2, "/samples/kick");
      --  Assign_Sample (ST_3, "/samples/snare");
      --  Assign_Sample (ST_4, "/samples/clap");
      --  Assign_Sample (ST_5, "/samples/rim");
      --  Assign_Sample (ST_6, "/samples/hho");
      --  Assign_Sample (ST_7, "/samples/hhc");
      --  Assign_Sample (ST_8, "/samples/crash");
      --  Assign_Sample (ST_9, "/samples/A4");
      --  Assign_Sample (ST_10, "/samples/B4");
      --  Assign_Sample (ST_11, "/samples/C5");
      --  Assign_Sample (ST_12, "/samples/D5");
      --  Assign_Sample (ST_13, "/samples/E5");
      --  Assign_Sample (ST_14, "/samples/F5");
      --  Assign_Sample (ST_15, "/samples/G5");
      --  Assign_Sample (ST_16, "/samples/A5");
      Assign_Sample (ST_1,  "/samples/A4");
      Assign_Sample (ST_2,  "/samples/B4");
      Assign_Sample (ST_3,  "/samples/C5");
      Assign_Sample (ST_4,  "/samples/D5");
      Assign_Sample (ST_5,  "/samples/E5");
      Assign_Sample (ST_6,  "/samples/F5");
      Assign_Sample (ST_7,  "/samples/G5");
      Assign_Sample (ST_8,  "/samples/A5");
      Assign_Sample (ST_9,  "/samples/A4");
      Assign_Sample (ST_10, "/samples/B4");
      Assign_Sample (ST_11, "/samples/C5");
      Assign_Sample (ST_12, "/samples/D5");
      Assign_Sample (ST_13, "/samples/E5");
      Assign_Sample (ST_14, "/samples/F5");
      Assign_Sample (ST_15, "/samples/G5");
      Assign_Sample (ST_16, "/samples/A5");
   end Load_Samples;

   -------------------
   -- Assign_Sample --
   -------------------

   procedure Assign_Sample (Track       : WNM.Tracks;
                            Sample_Path : String)
   is
   begin
      --  Sample_For_Track (Track) := Sample;

      Sample_Stream.Assign_Sample (Sample_Stream.To_Stream_Track (Track),
                                  Sample_Path);
   end Assign_Sample;

   ---------------------
   -- Sample_Of_Track --
   ---------------------

   function Sample_Of_Track (Track : WNM.Tracks)
                             return WNM.Sample_Library.Sample_Entry_Index
   is
      pragma Unreferenced (Track);
   begin
      return 1;
      -- return Sample_For_Track (Track);
   end Sample_Of_Track;

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
      Pan_For_Track (Track) := Pan_For_Track (Track) + Pan * 10;
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

   ------------
   -- Update --
   ------------

   function Update return WNM.Time.Time_Ms is

      procedure Process (Out_L, Out_R : out Mono_Buffer;
                         In_L,  In_R  :     Mono_Buffer);

      -------------
      -- Process --
      -------------

      procedure Process (Out_L, Out_R : out Mono_Buffer;
                         In_L,  In_R  :     Mono_Buffer)
      is
         Len : File_System.File_Signed_Size;

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
               Pan    := Float (Pan_For_Track (To_Track (ST))) / 100.0;
            end if;

            for Index in Mono_Samples'Range loop

               Sample := Float (Mono_Samples (Index));
               Sample := Sample * Volume;

               Right := Sample * (1.0 - Pan);
               Left  := Sample * (1.0 + Pan);

               Val := Integer_32 (Out_L (Index)) + Integer_32 (Left);
               if Val > Integer_32 (Mono_Sample'Last) then
                  Out_L (Index) := Mono_Sample'Last;
               elsif Val < Integer_32 (Integer_16'First) then
                  Out_L (Index) := Mono_Sample'First;
               else
                  Out_L (Index) := Mono_Sample (Val);
               end if;

               Val := Integer_32 (Out_R (Index)) + Integer_32 (Right);
               if Val > Integer_32 (Mono_Sample'Last) then
                  Out_R (Index) := Mono_Sample'Last;
               elsif Val < Integer_32 (Integer_16'First) then
                  Out_R (Index) := Mono_Sample'First;
               else
                  Out_R (Index) := Mono_Sample (Val);
               end if;
            end loop;
         end Mix;

      begin
         if Passthrough /= None then
            Out_R := In_R;
            Out_L := In_L;
         else
            Out_R := (others => 0);
            Out_L := (others => 0);
         end if;

         declare
            Sample_Buf : Mono_Buffer;
            Success : Boolean;
         begin
            for Track in Stream_Track loop
               Next_Buffer (Track, Sample_Buf, Success);
               if Success
                 and then
                  (Track = Always_On or else not Track_Muted (To_Track (Track)))
               then
                  Mix (Sample_Buf, Track);
               end if;
            end loop;
         end;

         -- Recording --
         if Recording_Source /= None then
            declare
               Sample_Buf : Mono_Buffer;
            begin
               case Recording_Source is
               when None =>
                  null;
               when Line_In | FM =>
                  Copy_Stereo_To_Mono (In_L, In_R, Sample_Buf);
               when Master_Output =>
                  Copy_Stereo_To_Mono (Out_L, Out_R, Sample_Buf);
               end case;

               Len := Write (Recording_File, Sample_Buf'Address, Sample_Buf'Length * 2);
               Recording_Size := Recording_Size + Len;
            end;
         end if;

         Glob_Sample_Clock := Glob_Sample_Clock + Samples_Per_Buffer;
      end Process;

      procedure Generate_Audio is new WNM.Audio.Generate_Audio (Process);

      Now : constant WNM.Time.Time_Ms := WNM.Time.Clock;
   begin
      if Now >= Next_Start then
         Next_Start := Next_Start + 0;

         Generate_Audio;
      end if;

      return Next_Start;
   end Update;

   ---------------------
   -- Set_Passthrough --
   ---------------------

   procedure Set_Passthrough (Kind : Input_Kind) is
   begin
      WNM.Audio.Select_Input (Kind);
      Passthrough := Kind;
   end Set_Passthrough;

   ---------------------
   -- Get_Passthrough --
   ---------------------

   function Get_Passthrough return Input_Kind is
   begin
      return Passthrough;
   end Get_Passthrough;

   -------------------
   -- Now_Recording --
   -------------------

   function Now_Recording return Rec_Source
   is (Recording_Source);

   ---------------------
   -- Start_Recording --
   ---------------------

   procedure Start_Recording (Filename : String;
                              Source   : Rec_Source;
                              Max_Size : Positive)
   is
      pragma Unreferenced (Max_Size);
   begin
      if Recording_Source /= None then
         return;
      end if;

      Recording_Source := Source;
      Create_File (Recording_File, Filename);
      Recording_Size := 0;
   end Start_Recording;

   --------------------
   -- Stop_Recording --
   --------------------

   procedure Stop_Recording is
   begin
      Close (Recording_File);
      Recording_Source := None;
   end Stop_Recording;

   -----------------
   -- Record_Size --
   -----------------

   function Record_Size return Natural
   is (Natural (Recording_Size));

end WNM.Synth;
