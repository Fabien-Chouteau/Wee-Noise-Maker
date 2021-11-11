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
with WNM.MIDI.Queues;
--  with Hex_Dump;
--  with Semihosting;

package body WNM.Synth is

   Recording_File   : aliased File_Descriptor;
   Recording_Source : Rec_Source;
   Recording_Size   : File_Signed_Size;

   Pan_For_Track : array (WNM.Tracks) of Integer := (others => 0)
     with Atomic_Components;
   Volume_For_Track : array (WNM.Tracks) of Integer := (others => 50)
     with Atomic_Components;
   --  Sample_For_Track : array (WNM.Tracks) of Sample_Entry_Index :=
   --    (others => Invalid_Sample_Entry)
   --    with Atomic_Components;

   Passthrough : Input_Kind := Line_In;

   Next_Start : WNM.Time.Time_Microseconds := WNM.Time.Time_Microseconds'First;
   Glob_Sample_Clock : Sample_Time := 0 with Volatile;

   procedure Copy_Stereo_To_Mono (L, R : Mono_Buffer;
                                  Dst : out Mono_Buffer);

   procedure Process_MIDI_Events;

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

   -----------
   -- Event --
   -----------

   procedure Event (Msg : MIDI.Message) is
      Track : constant WNM.Tracks := WNM.MIDI.To_Track (Msg.Chan);
   begin
      case Msg.Kind is
         when MIDI.Note_On =>
            Trig (Track);
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
      Assign_Sample (ST_1, "/samples/kick");
      Assign_Sample (ST_2, "/samples/snare");
      Assign_Sample (ST_3, "/samples/hh");
      Assign_Sample (ST_4, "/samples/clap");
      Assign_Sample (ST_5, "/samples/rim");
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
      for ST in ST_6 .. ST_16 loop
         Assign_Sample (ST, "samples/clap");
      end loop;
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

   -------------------------
   -- Process_MIDI_Events --
   -------------------------

   procedure Process_MIDI_Events is
      procedure Pop is new WNM.MIDI.Queues.Synth_Pop (Event);
   begin
      Pop;
   end Process_MIDI_Events;

   ------------
   -- Update --
   ------------

   function Update return WNM.Time.Time_Microseconds is

      procedure Process (Out_L, Out_R : out Mono_Buffer;
                         In_L,  In_R  :     Mono_Buffer);
      pragma Unreferenced (Process);
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
               if Success then
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
               when Line_In =>
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

      --  procedure Generate_Audio is new WNM.Audio.Generate_Audio (Process);

      Now : constant WNM.Time.Time_Microseconds := WNM.Time.Clock;
   begin
      if Now >= Next_Start then
         Next_Start := Next_Start + 0;

         Process_MIDI_Events;

         --  Generate_Audio;
      end if;

      return Next_Start;
   end Update;

   ------------------
   -- Next_Samples --
   ------------------

   procedure Next_Samples (Output : out Audio.Stereo_Buffer;
                           Input  :     Audio.Stereo_Buffer)
   is
      --  Len : File_System.File_Signed_Size;

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

            Val := Integer_32 (Output (Index).L) + Integer_32 (Left);
            if Val > Integer_32 (Mono_Sample'Last) then
               Output (Index).L := Mono_Sample'Last;
            elsif Val < Integer_32 (Integer_16'First) then
               Output (Index).L := Mono_Sample'First;
            else
               Output (Index).L := Mono_Sample (Val);
            end if;

            Val := Integer_32 (Output (Index).R) + Integer_32 (Right);
            if Val > Integer_32 (Mono_Sample'Last) then
               Output (Index).R := Mono_Sample'Last;
            elsif Val < Integer_32 (Integer_16'First) then
               Output (Index).R := Mono_Sample'First;
            else
               Output (Index).R := Mono_Sample (Val);
            end if;
         end loop;
      end Mix;

   begin
      if Passthrough /= None then
         Output := Input;
      else
         Output := (others => (0, 0));
      end if;

      declare
         Sample_Buf : Mono_Buffer;
         Success : Boolean;
      begin
         for Track in Stream_Track loop
            Next_Buffer (Track, Sample_Buf, Success);
            if Success then
               Mix (Sample_Buf, Track);
            end if;
         end loop;
      end;

      -- Recording --
      --  if Recording_Source /= None then
      --     declare
      --        Sample_Buf : Mono_Buffer;
      --     begin
      --        case Recording_Source is
      --           when None =>
      --              null;
      --           when Line_In =>
      --              Copy_Stereo_To_Mono (In_L, In_R, Sample_Buf);
      --           when Master_Output =>
      --              Copy_Stereo_To_Mono (Out_L, Out_R, Sample_Buf);
      --        end case;
      --
      --        Len := Write (Recording_File, Sample_Buf'Address, Sample_Buf'Length * 2);
      --        Recording_Size := Recording_Size + Len;
      --     end;
      --  end if;

      Glob_Sample_Clock := Glob_Sample_Clock + Samples_Per_Buffer;
   end Next_Samples;

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
