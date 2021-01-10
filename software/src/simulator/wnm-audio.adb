with GNAT.OS_Lib;
with Interfaces.C; use Interfaces.C;

with BBqueue;

with System.Storage_Elements; use System.Storage_Elements;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

package body WNM.Audio is

   procedure SDL_Audio_Callback (Userdata : System.Address;
                                 Stream   : System.Address;
                                 Len      : Interfaces.C.int);
   pragma Export (C, SDL_Audio_Callback, "sdl_audio_callback");

   function Init_SDL_Audio (Sample_Rate : Interfaces.C.int;
                            Sample_Cnt  : Interfaces.C.int)
                            return Interfaces.C.int;
   pragma Import (C, Init_SDL_Audio, "init_sdl_audio");

   Buffer_Count : constant BBqueue.Buffer_Offset := Audio_Queue_Size;

   Out_L : array (1 .. Buffer_Count) of Mono_Buffer;
   Out_R : array (1 .. Buffer_Count) of Mono_Buffer;
   In_L : array (1 .. Buffer_Count) of Mono_Buffer;
   In_R : array (1 .. Buffer_Count) of Mono_Buffer;

   Out_Queue : BBqueue.Offsets_Only (Buffer_Count);
   In_Queue : BBqueue.Offsets_Only (Buffer_Count);

   Radio_Input_File : Ada.Streams.Stream_IO.File_Type;
   Radio_Input_Stream : Ada.Streams.Stream_IO.Stream_Access;

   ------------------------
   -- SDL_Audio_Callback --
   ------------------------

   procedure SDL_Audio_Callback (Userdata : System.Address;
                                 Stream   : System.Address;
                                 Len      : Interfaces.C.int)
   is
      use BBqueue;

      pragma Unreferenced (Userdata);
      Output : Stereo_Buffer with Address => Stream;

      Out_Read : Read_Grant;
      In_Write : Write_Grant;

   begin
      if Len /= WNM.Samples_Per_Buffer * 2 * 2 then
         raise Program_Error with "Unexpected SDL buffer len:" & Len'Img;
      end if;

      -- Output --

      Read (Out_Queue, Out_Read, 1);

      if State (Out_Read) /= Valid then
         Output := (others => (0, 0));
      else

         declare
            Out_Index : constant Buffer_Offset :=
              Out_L'First + Slice (Out_Read).From;
         begin
            for X in Output'Range loop
               Output (X).L := Out_L (Out_Index) (X);
               Output (X).R := Out_R (Out_Index) (X);
            end loop;
         end;

         Release (Out_Queue, Out_Read, 1);
      end if;

      -- Input --

      Grant (In_Queue, In_Write, 1);

      if State (In_Write) /= Valid then
         return;
      end if;

      declare
         In_Index : constant Buffer_Offset :=
           In_L'First + Slice (In_Write).From;

         Radio : Stereo_Buffer;
      begin

         Radio := Stereo_Buffer'Input (Radio_Input_Stream);

         for X in Output'Range loop
            In_L (In_Index) (X) := Radio (X).L;
            In_R (In_Index) (X) := Radio (X).R;
         end loop;

         Commit (In_Queue, In_Write, 1);
      end;
   end SDL_Audio_Callback;

   --------------------
   -- Generate_Audio --
   --------------------

   procedure Generate_Audio is
      use BBqueue;

      In_Read : Read_Grant;
      Out_Write : Write_Grant;
   begin
      Grant (Out_Queue, Out_Write, 1);

      if State (Out_Write) /= Valid then
         return;
      end if;

      BBqueue.Read (In_Queue, In_Read, 1);

      if State (In_Read) /= Valid then
         Commit (Out_Queue, Out_Write, 0);
         return;
      end if;

      declare
         Out_Index : constant Buffer_Offset :=
           Out_L'First + Slice (Out_Write).From;

         In_Index : constant Buffer_Offset :=
           In_L'First +  Slice (Out_Write).From;
      begin
         Process (Out_L (Out_Index),
                  Out_R (Out_Index),
                  In_L (In_Index),
                  In_R (In_Index));

         Commit (Out_Queue, Out_Write, 1);
         Release (In_Queue, In_Read, 1);
      end;
   end Generate_Audio;

   ------------------
   -- Select_Input --
   ------------------

   procedure Select_Input (Kind : Input_Kind) is
   begin
      null;
   end Select_Input;

   ----------------
   -- Set_Volume --
   ----------------

   procedure Set_Volume (Volume : DAC_Volume) is
   begin
      null;
   end Set_Volume;

begin

   Ada.Streams.Stream_IO.Open (Radio_Input_File,
                               Ada.Streams.Stream_IO.In_File,
                               "/home/chouteau/Downloads/radio_in.raw");


   Radio_Input_Stream := Ada.Streams.Stream_IO.Stream (Radio_Input_File);

   if Radio_Input_Stream = null then
      raise Program_Error with "Radio stream error...";
   end if;

   if GNAT.OS_Lib.Getenv ("OS").all = "Windows_NT" then -- Memory leak right here...
      GNAT.OS_Lib.Setenv ("SDL_AUDIODRIVER", "directsound");
   end if;

   if Init_SDL_Audio (Sample_Frequency, Samples_Per_Buffer) /= 0 then
      raise Program_Error with "SDL Audio init failed";
   end if;

end WNM.Audio;
