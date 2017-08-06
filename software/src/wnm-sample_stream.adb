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
     with Priority => Sample_Task_Priority;

   Start_Task : Suspension_Object;

   procedure Close_Stream (ID : Valid_Stream_ID);
   procedure Process_Request;

   ------------------
   -- Streams_Prot --
   ------------------

   protected body Streams_Prot is

      -----------------
      -- Next_Buffer --
      -----------------

      procedure Next_Buffer (ID     : Valid_Stream_ID;
                             Buffer : out Any_Managed_Buffer;
                             Track  : out Tracks)
      is
      begin
         Pop (FIFOs (ID), Buffer);
         Track := On_Track (ID);
         Something_To_Do := True;
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
      -- Close --
      -----------

      procedure Close (ID : Valid_Stream_ID) is
         Buf : Any_Managed_Buffer;
      begin

         loop
            Pop (FIFOs (ID), Buf);
            exit when Buf = null;
            Release_Buffer (Buf);
         end loop;

         Is_Free (ID) := True;
      end Close;

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
         null;
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

         Something_To_Do := Cnt /= 0 or else not Request_FIFO.Empty (Requests);

      end Check_If_Theres_Something_To_Do;

      --------------
      -- Allocate --
      --------------

      procedure Allocate (ID    : out Stream_ID;
                          Track : Tracks) is
      begin
         ID := Valid_Stream_ID'First;

         loop
            exit when Is_Free (ID);

            if ID = Valid_Stream_ID'Last then
               ID := Invalid_Stream;
               return;
            else
               ID := ID + 1;
            end if;
         end loop;

         Is_Free (ID)       := False;
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
         else

            Tmp := Req.Filepath;
            --  Discard this request...
            Free (Tmp);
         end if;
         Something_To_Do := True;
      end Push_Request;

      -----------------
      -- Pop_Request --
      -----------------

      procedure Pop_Request (Req : out Sample_Request) is
      begin
         Request_FIFO.Pop (Requests, Req);
      end Pop_Request;

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
                    Track       : Tracks;
                    Looping     : Boolean)
   is
      Req : Sample_Request;
   begin
      Req.Start_Point := File_Size (Start_Point);
      Req.End_Point   := File_Size (End_Point);
      Req.Track       := Track;
      Req.Looping     := Looping;
      Req.Filepath    := new String'(Filepath);
      Streams_Prot.Push_Request (Req);
   end Start;

   -----------------
   -- Next_Buffer --
   -----------------

   procedure Next_Buffer (ID     : Stream_ID;
                          Buffer : out Any_Managed_Buffer;
                          Track  : out Tracks)
   is
   begin
      if ID = Invalid_Stream then
         Buffer := null;
      else
         Streams_Prot.Next_Buffer (ID, Buffer, Track);
      end if;
   end Next_Buffer;

   ------------------
   -- Close_Stream --
   ------------------

   procedure Close_Stream (ID : Valid_Stream_ID) is
   begin
      Streams_Prot.Close (ID);
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
         Streams_Prot.Allocate (ID, Req.Track);
         if ID /= Invalid_Stream then

            if Streams (ID).State /= Unused then
               Close_Stream (ID);
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

            Amount := Streams (ID).Start_Point;
            Status := Seek (Streams (ID).FD, From_Start, Amount);
            if Status /= OK then
               raise Program_Error with "Seek error";
            end if;

            Streams (ID).Offset := Amount;
            Streams (ID).State := In_Progress;

         end if;
         Free (Req.Filepath);
      end if;
   end Process_Request;

   ------------------------
   -- Sample_Stream_Task --
   ------------------------

   task body Sample_Stream_Task is
      Buffer : Any_Managed_Buffer;
      Amount : File_Size;
      Status : Status_Code;
      Need   : Boolean;
   begin
      Suspend_Until_True (Start_Task);
      loop
         Streams_Prot.Check_If_Theres_Something_To_Do;
         Streams_Prot.Suspend_While_Nothing_To_Do;

         --  Process at most one sample request
         Process_Request;

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
                        Close_Stream (ID);
                     end if;
                     Release_Buffer (Buffer);
                  else

                     Streams_Prot.Push (ID, Buffer);
                  end if;
               end if;
            end if;
         end loop;
      end loop;
   end Sample_Stream_Task;

end WNM.Sample_Stream;
