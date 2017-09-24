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

with WNM.Buffer_FIFO;              use WNM.Buffer_FIFO;
with File_IO;                      use File_IO;
with WNM.Buffer_Allocation;        use WNM.Buffer_Allocation;
with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;
with Ada.Unchecked_Deallocation;

package body WNM.Sample_Stream is

   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

   task Sample_Stream_Task
     with Priority     => Sample_Task_Priority,
          Storage_Size => Sample_Taks_Stack_Size;

   Start_Task      : Suspension_Object;
   Log_Record_Size : Natural := 0;

   procedure Close_Stream (ID             : Valid_Stream_ID;
                           Keep_Allocated : Boolean);
   procedure Process_Request;
   procedure Process_Record_Request (FD  : in out File_Descriptor);

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
   -- Streams_Prot --
   ------------------

   protected body Streams_Prot is

      -----------------
      -- Next_Buffer --
      -----------------

      procedure Next_Buffer (ID     : Valid_Stream_ID;
                             Buffer : out Any_Managed_Buffer;
                             Track  : out Stream_Track)
      is
      begin
         Pop (FIFOs (ID), Buffer);
         Track := On_Track (ID);
         if not Is_Free (ID) then
            Something_To_Do := True;
         end if;
      end Next_Buffer;

      ----------
      -- Push --
      ----------

      procedure Push (ID     : Valid_Stream_ID;
                      Buffer : Any_Managed_Buffer)
      is
      begin
         Push (FIFOs (ID), Buffer);
      end Push;

      -----------
      -- Flush --
      -----------

      procedure Flush (ID      : Valid_Stream_ID;
                       Free_It : Boolean)
      is
         Buf : Any_Managed_Buffer;
      begin

         loop
            Pop (FIFOs (ID), Buf);
            exit when Buf = null;
            Release_Buffer (Buf);
         end loop;

         if Free_It then
            Is_Free (ID) := True;
         end if;
      end Flush;

      ----------------------
      -- Need_More_Buffer --
      ----------------------

      procedure Need_More_Buffer (ID   : Valid_Stream_ID;
                                  Need : out Boolean)
      is
      begin
         Need := not Is_Free (ID) and then not Full (FIFOs (ID));
      end Need_More_Buffer;


      ---------------------------------
      -- Suspend_While_Nothing_To_Do --
      ---------------------------------

      entry Suspend_While_Nothing_To_Do when Something_To_Do is
      begin
         Something_To_Do := False;
      end Suspend_While_Nothing_To_Do;

      -------------------------------------
      -- Check_If_Theres_Something_To_Do --
      -------------------------------------

      procedure Check_If_Theres_Something_To_Do is
         Cnt : Natural := 0;
      begin

         --  Count how many FIFO still need more buffers
         for ID in Valid_Stream_ID loop

            if not Is_Free (ID) and then not Full (FIFOs (ID)) then
               Cnt := Cnt + 1;
            end if;
         end loop;

         Something_To_Do :=
             Cnt /= 0
           or else
             not Request_FIFO.Empty (Requests)
           or else
             (Rec_Src /= None and then not Empty (Rec_FIFO));

      end Check_If_Theres_Something_To_Do;

      --------------
      -- Allocate --
      --------------

      procedure Allocate (ID    : out Stream_ID;
                          Track : Stream_Track;
                          Reuse : Boolean) is
         Ret : Stream_ID;
      begin
         ID := Valid_Stream_ID'First;

         if Reuse then

            Ret := Invalid_Stream;

            loop
               if Is_Free (ID) then
                  Ret := ID;
               elsif On_Track (ID) = Track then
                  --  Reuse the stream allocated for this channel
                  Ret := ID;
                  exit;
               end if;

               if ID = Valid_Stream_ID'Last then
                  exit;
               end if;
               ID := ID + 1;
            end loop;

            if Ret = Invalid_Stream then
               return;
            else
               ID := Ret;
            end if;
         else
            loop
               exit when Is_Free (ID);

               if ID = Valid_Stream_ID'Last then
                  ID := Invalid_Stream;
                  return;
               else
                  ID := ID + 1;
               end if;
            end loop;
         end if;

         Is_Free (ID)    := False;
         On_Track (ID)   := Track;
         Something_To_Do := True;
      end Allocate;

      ------------------
      -- Push_Request --
      ------------------

      procedure Push_Request (Req : Sample_Request) is
         Tmp : String_Access;
      begin
         if not Request_FIFO.Full (Requests) then
            Request_FIFO.Push (Requests, Req);
            Something_To_Do := True;
         else

            Tmp := Req.Filepath;
            --  Discard this request...
            Free (Tmp);
         end if;
      end Push_Request;

      -----------------
      -- Pop_Request --
      -----------------

      procedure Pop_Request (Req : out Sample_Request) is
      begin
         Request_FIFO.Pop (Requests, Req);
      end Pop_Request;

      ---------------------
      -- Set_Rec_Request --
      ---------------------

      procedure Set_Rec_Request (Rec : Record_Request) is
         Tmp : String_Access;
      begin
         if not Rec_Request.Active then
            Rec_Request        := Rec;
            Rec_Src            := Rec_Request.Src;
            Rec_Request.Active := True;
            Something_To_Do    := True;
         else
            --  Discard the request
            Tmp := Rec.Filepath;
            Free (Tmp);
         end if;
      end Set_Rec_Request;

      ---------------------
      -- Get_Rec_Request --
      ---------------------

      procedure Get_Rec_Request (Rec : out Record_Request) is
      begin
         Rec                := Rec_Request;
         Rec_Request.Active := False;
      end Get_Rec_Request;

      -------------------
      -- Now_Recording --
      -------------------

      function Now_Recording return Rec_Source is
      begin
         return Rec_Src;
      end Now_Recording;

      ---------------------
      -- Start_Recording --
      ---------------------

      procedure Start_Recording (Src : Rec_Source) is
      begin
         Rec_Src := Src;
      end Start_Recording;

      --------------------
      -- Stop_Recording --
      --------------------

      procedure Stop_Recording is
         Buffer : Any_Managed_Buffer;
      begin

         --  Clear the FIFO
         loop
            Pop (Rec_FIFO, Buffer);
            exit when Buffer = null;
            WNM.Buffer_Allocation.Release_Buffer (Buffer);
         end loop;

         Rec_Src := None;
         Something_To_Do := True;
      end Stop_Recording;

      ------------------------
      -- Push_Record_Buffer --
      ------------------------

      procedure Push_Record_Buffer (Buffer : not null Any_Managed_Buffer) is
      begin
         if Rec_Src /= None and then not Full (Rec_FIFO) then
            Push (Rec_FIFO, Buffer);
            Something_To_Do := True;
         else
            WNM.Buffer_Allocation.Release_Buffer (Buffer);
         end if;
      end Push_Record_Buffer;

      -----------------------
      -- Get_Record_Buffer --
      -----------------------

      procedure Get_Record_Buffer (Buffer : out Any_Managed_Buffer) is
      begin
         Pop (Rec_FIFO, Buffer);
      end Get_Record_Buffer;

   end Streams_Prot;

   ------------------------------
   -- Start_Sample_Stream_Task --
   ------------------------------

   procedure Start_Sample_Stream_Task is
   begin
      Set_True (Start_Task);
   end Start_Sample_Stream_Task;

   -----------
   -- Start --
   -----------

   procedure Start (Filepath    : String;
                    Start_Point : Natural;
                    End_Point   : Natural;
                    Track       : Stream_Track;
                    Looping     : Boolean;
                    Poly        : Boolean)
   is
      Req : Sample_Request;
   begin
      Req.Start_Point := File_Size (Start_Point);
      Req.End_Point   := File_Size (End_Point);
      Req.Track       := Track;
      Req.Looping     := Looping;
      Req.Poly        := Poly;
      Req.Filepath    := new String'(Filepath);
      Streams_Prot.Push_Request (Req);
   end Start;

   -----------------
   -- Next_Buffer --
   -----------------

   procedure Next_Buffer (ID     : Stream_ID;
                          Buffer : out Any_Managed_Buffer;
                          Track  : out Stream_Track)
   is
   begin
      if ID = Invalid_Stream then
         Buffer := null;
      else
         Streams_Prot.Next_Buffer (ID, Buffer, Track);
      end if;
   end Next_Buffer;

   -------------------
   -- Now_Recording --
   -------------------

   function Now_Recording return Rec_Source is
   begin
      return Streams_Prot.Now_Recording;
   end Now_Recording;

   ---------------------
   -- Start_Recording --
   ---------------------

   procedure Start_Recording (Filename : String;
                              Source   : Rec_Source;
                              Max_Size : Positive)
   is
      Req : Record_Request;
   begin
      Req.Active := True;
      Req.Src := Source;
      Req.Max_Size := File_IO.File_Size (Max_Size);
      Req.Filepath := new String'(Filename);
      Streams_Prot.Set_Rec_Request (Req);
   end Start_Recording;

   --------------------
   -- Stop_Recording --
   --------------------

   procedure Stop_Recording is
   begin
      Streams_Prot.Stop_Recording;
   end Stop_Recording;

   -----------------
   -- Record_Size --
   -----------------

   function Record_Size return Natural is
   begin
      return Log_Record_Size;
   end Record_Size;

   ------------------------
   -- Push_Record_Buffer --
   ------------------------

   procedure Push_Record_Buffer (Buffer : not null Any_Managed_Buffer) is
   begin
      Streams_Prot.Push_Record_Buffer (Buffer);
   end Push_Record_Buffer;

   ------------------
   -- Close_Stream --
   ------------------

   procedure Close_Stream (ID             : Valid_Stream_ID;
                           Keep_Allocated : Boolean) is
   begin
      Streams_Prot.Flush (ID, Free_It => not Keep_Allocated);
      Close (Streams (ID).FD);
      Streams (ID).State := Unused;
   end Close_Stream;

   ---------------------
   -- Process_Request --
   ---------------------

   procedure Process_Request is
      Req    : Sample_Request;
      ID     : Stream_ID;
      Status : Status_Code;
      Amount : File_Size;
   begin

      Streams_Prot.Pop_Request (Req);
      if Req.Filepath /= null then
         Streams_Prot.Allocate (ID, Req.Track, Reuse => not Req.Poly);
         if ID /= Invalid_Stream then

            if Streams (ID).State /= Unused then
               Close_Stream (ID, Keep_Allocated => True);
            end if;

            Streams (ID).Start_Point := Req.Start_Point;
            Streams (ID).End_Point   := Req.End_Point;
            Streams (ID).Track       := Req.Track;
            Streams (ID).Looping     := Req.Looping;

            --  Open File
            Status := Open (Streams (ID).FD, Req.Filepath.all, Read_Only);

            if Status /= OK then
               raise Program_Error with "Cannot open '" & Req.Filepath.all &
                 "': " & Status'Img;
            end if;

            Streams (ID).Size := Size (Streams (ID).FD);

            if Streams (ID).Start_Point >= Streams (ID).Size then
               Close_Stream (ID, Keep_Allocated => False);
            else
               Amount := Streams (ID).Start_Point;
               Status := Seek (Streams (ID).FD, From_Start, Amount);
               if Status /= OK then
                  raise Program_Error with "Seek error";
               end if;

               Streams (ID).Offset := Amount;
               Streams (ID).State := In_Progress;
            end if;

         end if;
         Free (Req.Filepath);
      end if;
   end Process_Request;

   ----------------------------
   -- Process_Record_Request --
   ----------------------------

   procedure Process_Record_Request (FD  : in out File_Descriptor) is
      Req    : Record_Request;
      Status : Status_Code;
   begin
      Streams_Prot.Get_Rec_Request (Req);

      if Req.Active and then Req.Filepath /= null then

         Status := Open (FD, Req.Filepath.all, Write_Only);
         if Status /= OK then
            raise Program_Error with "Cannot open '" & Req.Filepath.all &
              "': " & Status'Img;
         end if;

         Streams_Prot.Start_Recording (Req.Src);
         Free (Req.Filepath);
         Log_Record_Size := 0;
      end if;
   end Process_Record_Request;

   ------------------------
   -- Sample_Stream_Task --
   ------------------------

   task body Sample_Stream_Task is
      Buffer : Any_Managed_Buffer;
      Amount : File_Size;
      Status : Status_Code;
      Need   : Boolean;

      Rec_FD : File_Descriptor;
   begin
      Suspend_Until_True (Start_Task);
      loop
         Streams_Prot.Check_If_Theres_Something_To_Do;
         Streams_Prot.Suspend_While_Nothing_To_Do;

         --  Process at most one sample request
         Process_Request;

         --  Play samples
         for ID in Valid_Stream_ID loop
            Streams_Prot.Need_More_Buffer (ID, Need);
            if Need then
               Buffer := Allocate (RAM, 512 * 2);
               if Buffer = null then
                  raise Program_Error with "Cannot allocate sample buffer...";
               else
                  Amount := Read (Streams (ID).FD,
                                  Buffer.Buffer_Address,
                                  File_Size (Buffer.Buffer_Length));

                  Streams (ID).Offset := Streams (ID).Offset + Amount;

                  if Amount /= File_Size (Buffer.Buffer_Length)
                    or else
                      Streams (ID).Offset > Streams (ID).End_Point
                  then

                     --  End of file or end of sample limit

                     if Streams (ID).Looping then
                        Streams (ID).Offset := Streams (ID).Start_Point;
                        Amount := Streams (ID).Start_Point;
                        Status := Seek (Streams (ID).FD, From_Start, Amount);
                        if Status /= OK then
                           raise Program_Error with "Seek error";
                        end if;
                     else
                        Close_Stream (ID, Keep_Allocated => False);
                     end if;
                     Release_Buffer (Buffer);
                  else

                     Streams_Prot.Push (ID, Buffer);
                  end if;
               end if;
            end if;
         end loop;

         --  Record
         if Is_Open (Rec_FD) then

            --  We are already recording

            if Streams_Prot.Now_Recording = None then
               --  Recording is now finised
               Close (Rec_FD);
            else
               declare
                  Buffer : Any_Managed_Buffer;
               begin
                  Streams_Prot.Get_Record_Buffer (Buffer);
                  if Buffer /= null then
                     if Write (Rec_FD,
                               Buffer.Buffer_Address,
                               File_Size (Buffer.Buffer_Length))
                       /= File_Size (Buffer.Buffer_Length)
                     then
                        raise Program_Error with "Record Write failure";
                     end if;

                     Log_Record_Size := Log_Record_Size +
                       Natural (Buffer.Buffer_Length);

                     Release_Buffer (Buffer);
                  end if;
               end;
            end if;
         else
            --  We are not recording, let's see if we have a request...
            Process_Record_Request (Rec_FD);
         end if;
      end loop;
   end Sample_Stream_Task;

end WNM.Sample_Stream;
