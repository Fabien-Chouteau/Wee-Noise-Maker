-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2017 Fabien Chouteau                    --
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

with Ada.Text_IO;

with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

with WNM.File_System; use WNM.File_System;
with Littlefs;

package body WNM.Sample_Stream is

   procedure Close_Stream (ST : Stream_Track);

   ---------------------
   -- To_Stream_Track --
   ---------------------

   function To_Stream_Track (T : Tracks) return Stream_Track
   is (case T is
          when B1 => ST_1,
          when B2 => ST_2,
          when B3 => ST_3,
          when B4 => ST_4,
          when B5 => ST_5,
          when B6 => ST_6,
          when B7 => ST_7,
          when B8 => ST_8,
          when B9 => ST_9,
          when B10 => ST_10,
          when B11 => ST_11,
          when B12 => ST_12,
          when B13 => ST_13,
          when B14 => ST_14,
          when B15 => ST_15,
          when B16 => ST_16);

   --------------
   -- To_Track --
   --------------

   function To_Track (ST : Stream_Track) return Tracks
   is (case ST is
          when ST_1   => B1,
          when ST_2   => B2,
          when ST_3   => B3,
          when ST_4   => B4,
          when ST_5   => B5,
          when ST_6   => B6,
          when ST_7   => B7,
          when ST_8   => B8,
          when ST_9   => B9,
          when ST_10  => B10,
          when ST_11  => B11,
          when ST_12  => B12,
          when ST_13  => B13,
          when ST_14  => B14,
          when ST_15  => B15,
          when others => B16);

   ------------------
   -- Close_Stream --
   ------------------

   procedure Close_Stream (ST : Stream_Track) is
   begin
      if Streams (St).State /= Unused then

         Close (Streams (St).FD);

         Streams (St).State := Unused;
      end if;
   end Close_Stream;

   -------------------
   -- Assign_Sample --
   -------------------

   procedure Assign_Sample (Track    : Stream_Track;
                            Filepath : String)
   is
   begin
      Close_Stream (Track);

      Open_Read (Streams (Track).FD, Filepath);

      Streams (Track).State := Assigned;
   end Assign_Sample;

   -----------
   -- Start --
   -----------

   procedure Start (Track       : Stream_Track;
                    Start_Point : Natural;
                    End_Point   : Natural;
                    Looping     : Boolean)
   is
   begin
      if Streams (Track).State = Unused then
         return;
      end if;

      Streams (Track).State := Running;

      Streams (Track).Looping := Looping;
      Streams (Track).Start_Point := Start_Point;
      Streams (Track).Cursor := Start_Point;
      Streams (Track).End_Point := End_Point;

      Seek (Streams (Track).FD, Offset (Start_Point), Littlefs.LFS_SEEK_SET);
   end Start;

   -----------------
   -- Next_Buffer --
   -----------------

   procedure Next_Buffer (Track   :     Stream_Track;
                          Buffer  : out Audio.Mono_Buffer;
                          Success : out Boolean)
   is
      Len : File_Signed_Size;
   begin
      if Streams (Track).State /= Running then
         Success := False;
         return;
      end if;

      Len := Read (Streams (Track).FD, Buffer'Address, Buffer'Length * 2);

      Streams (Track).Cursor := Streams (Track).Cursor + Natural (Len);

      if Len /= Buffer'Length * 2 then
         Success := False;
      else
         Success := True;
      end if;

      if not Success
        or else
          Streams (Track).Cursor > Streams (Track).End_Point
      then
         Streams (Track).State := Assigned;
      end if;
   end Next_Buffer;


   ---------------
   -- Copy_File --
   ---------------

   procedure Copy_File (Srcpath  : String;
                        From, To : Natural;
                        Dstpath  : String)
   is
      Src_FD, Dst_FD : aliased File_Descriptor;
      Buf : Storage_Array (1 .. 512);

      Len, Count : File_Signed_Size;
   begin
      Open_Read (Src_FD, Srcpath);
      Create_File (Dst_FD, Dstpath);

      Seek (Src_FD, Offset (From), Littlefs.LFS_SEEK_SET);

      Count := File_Signed_Size (From);

      loop
         Len := Read (Src_FD, Buf'Address, Buf'Length);

         exit when Len < 0;

         Len := Write (Dst_FD, Buf'Address, File_Size (Len));

         Count := Count + Len;
         exit when Len /= Buf'Length or else Count >= File_Signed_Size (To);
      end loop;

      Ada.Text_IO.Put_Line (Count'Img & " copied from '" &
                              Srcpath & "' to '" & Dstpath & "'");
      Close (Src_FD);
      Close (Dst_FD);
   end Copy_File;

end WNM.Sample_Stream;
