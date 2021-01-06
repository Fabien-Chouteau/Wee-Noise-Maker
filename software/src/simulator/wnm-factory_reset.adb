with Littlefs; use Littlefs;
with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;

with Ada.Streams.Stream_IO;

with Ada.Streams; use Ada.Streams;

package body WNM.Factory_Reset is

   procedure Copy_From_Host (FS : aliased in out Littlefs.LFS_T;
                             Hostpath, Localpath : String);

   --------------------
   -- Copy_From_Host --
   --------------------

   procedure Copy_From_Host (FS : aliased in out Littlefs.LFS_T;
                             Hostpath, Localpath : String) is
      Local_FD : aliased LFS_File;

      Host_File : Ada.Streams.Stream_IO.File_Type;
      Host_Stream : Ada.Streams.Stream_IO.Stream_Access;

      Buffer : Stream_Element_Array (1 .. 512);
      Last   : Stream_Element_Offset;
      Len : Littlefs.LFS_Signed_Size;
   begin
      if Littlefs.Open (FS, Local_FD, Localpath, LFS_O_WRONLY + LFS_O_CREAT) /= 0 then
         raise Program_Error with "Create file error...";
      end if;

      Ada.Streams.Stream_IO.Open (Host_File,
                                  Ada.Streams.Stream_IO.In_File,
                                  Hostpath);

      Host_Stream := Ada.Streams.Stream_IO.Stream (Host_File);

      loop

         Host_Stream.Read (Buffer, Last);

         exit when Last < Buffer'Last;

         Len :=  Write (FS, Local_FD,
                        Buffer'Address,
                        Buffer'Length);

         if Len /= Buffer'Length then
            raise Program_Error with "Copy_From_Host error:" & Len'Img;
         end if;

      end loop;

      if Close (FS, Local_FD) /= 0 then
         raise Program_Error with "Close error...";
      end if;

      Ada.Streams.Stream_IO.Close (Host_File);

   end Copy_From_Host;

   -----------
   -- Reset --
   -----------

   procedure Reset (FS : aliased in out Littlefs.LFS_T) is
   begin
      if Littlefs.Mkdir (FS, "samples") /= 0 then
         raise Program_Error with "Mkdir error...";
      end if;

      if Littlefs.Mkdir (FS, "projects") /= 0 then
         raise Program_Error with "Mkdir error...";
      end if;

      Copy_From_Host (FS, "/tmp/wnm-samples/BT0A0A7.WAV.raw", "/samples/kick");
      Copy_From_Host (FS, "/tmp/wnm-samples/ST0T0S0.WAV.raw", "/samples/snare");
      Copy_From_Host (FS, "/tmp/wnm-samples/HANDCLP1.WAV.raw", "/samples/clap");
      Copy_From_Host (FS, "/tmp/wnm-samples/RIM63.WAV.raw", "/samples/rim");
      Copy_From_Host (FS, "/tmp/wnm-samples/HHOD6.WAV.raw", "/samples/hho");
      Copy_From_Host (FS, "/tmp/wnm-samples/HHCD6.WAV.raw", "/samples/hhc");
      Copy_From_Host (FS, "/tmp/wnm-samples/CSHD4.WAV.raw", "/samples/crash");
      Copy_From_Host (FS, "/tmp/wnm-samples/RIDED6.WAV.raw", "/samples/ride");

   end Reset;

end WNM.Factory_Reset;
