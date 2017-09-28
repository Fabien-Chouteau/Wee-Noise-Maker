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

with Managed_Buffers;  use Managed_Buffers;
with WNM.Buffer_FIFO;
private with File_IO;
private with WNM.FIFO;

package WNM.Sample_Stream is

   procedure Start_Sample_Stream_Task;

   -------------
   -- Samples --
   -------------

   type Stream_ID is range 0 .. 10;

   Invalid_Stream : constant Stream_ID := Stream_ID'First;

   subtype Valid_Stream_ID is Stream_ID range
     Invalid_Stream + 1 .. Stream_ID'Last;

   type Stream_Track is (Always_On, ST_1, ST_2, ST_3, ST_4, ST_5, ST_6, ST_7,
                         ST_8, ST_9, ST_10, ST_11, ST_12, ST_13, ST_14, ST_15,
                         ST_16);

   function To_Stream_Track (T : Tracks) return Stream_Track;
   function To_Track (ST : Stream_Track) return Tracks
     with Pre => ST /= Always_On;

   procedure Start (Filepath    : String;
                    Start_Point : Natural;
                    End_Point   : Natural;
                    Track       : Stream_Track;
                    Looping     : Boolean;
                    Poly        : Boolean);

   procedure Next_Buffer (ID     : Stream_ID;
                          Buffer : out Any_Managed_Buffer;
                          Track  : out Stream_Track);

   procedure Copy_File (Srcpath  : String;
                        From, To : Natural;
                        Dstpath  : String);

   ---------------
   -- Recording --
   ---------------

   type Rec_Source is (None, Input, Master_Output);

   function Now_Recording return Rec_Source;

   procedure Start_Recording (Filename : String;
                              Source   : Rec_Source;
                              Max_Size : Positive)
     with Pre => Now_Recording = None and then Source /= None;

   procedure Stop_Recording;

   function Record_Size return Natural;

   procedure Push_Record_Buffer (Buffer : not null Any_Managed_Buffer);

private


   type String_Access is access all String;

   type Request_Kind is (Sample_Play, Copy_File);

   type Sample_Request (Kind : Request_Kind := Sample_Play) is record
      case Kind is
         when Sample_Play =>
            Filepath    : String_Access := null;
            Track       : Stream_Track;
            Start_Point : File_IO.File_Size;
            End_Point   : File_IO.File_Size;
            Looping     : Boolean;
            Poly        : Boolean;
         when Copy_File =>
            Srcpath     : String_Access := null;
            Dstpath     : String_Access := null;
            From, To    : Natural;
      end case;
   end record;

   No_Request : constant Sample_Request := (Kind        => Sample_Play,
                                            Filepath    => null,
                                            Track       => Always_On,
                                            Start_Point => 0,
                                            End_Point   => 0,
                                            Looping     => False,
                                            Poly        => False);

   package Request_FIFO is new WNM.FIFO (Element     => Sample_Request,
                                         Empty_Value => No_Request);

   type FIFO_Array is array (Valid_Stream_ID) of WNM.Buffer_FIFO.FIFO (3);
   type Free_Array is array (Valid_Stream_ID) of Boolean;
   type Track_Array is array (Valid_Stream_ID) of Stream_Track;

   type Record_Request is record
      Active   : Boolean := False;
      Filepath : String_Access := null;
      Src      : Rec_Source;
      Max_Size : File_IO.File_Size;
   end record;

   protected Streams_Prot
     with Priority => DAC_Task_Priority
   is

      -------------
      -- Samples --
      -------------

      procedure Next_Buffer (ID     : Valid_Stream_ID;
                             Buffer : out Any_Managed_Buffer;
                             Track  : out Stream_Track);

      procedure Push (ID     : Valid_Stream_ID;
                      Buffer : Any_Managed_Buffer);

      procedure Flush (ID      : Valid_Stream_ID;
                       Free_It : Boolean);

      procedure Need_More_Buffer (ID   : Valid_Stream_ID;
                                  Need : out Boolean);

      entry Suspend_While_Nothing_To_Do;

      procedure Check_If_Theres_Something_To_Do;

      procedure Allocate (ID    : out Stream_ID;
                          Track : Stream_Track;
                          Reuse : Boolean);

      procedure Push_Request (Req : Sample_Request);
      procedure Pop_Request (Req : out Sample_Request);

      ---------------
      -- Recording --
      ---------------

      procedure Set_Rec_Request (Rec : Record_Request);
      procedure Get_Rec_Request (Rec : out Record_Request);
      function Now_Recording return Rec_Source;
      procedure Start_Recording (Src : Rec_Source);
      procedure Stop_Recording;
      procedure Push_Record_Buffer (Buffer : not null Any_Managed_Buffer);
      procedure Get_Record_Buffer (Buffer : out Any_Managed_Buffer);

   private

      FIFOs     : FIFO_Array;
      Is_Free   : Free_Array := (others => True);
      On_Track  : Track_Array;

      Something_To_Do : Boolean := False;

      Requests  : Request_FIFO.FIFO (10);

      Rec_FIFO    : WNM.Buffer_FIFO.FIFO (2);
      Rec_Size    : Natural := 0;
      Rec_Src     : Rec_Source;
      Rec_Request : Record_Request;
   end Streams_Prot;

   type Stream_State is (Unused, In_Progress);

   type Stream_Info is record
      Start_Point : File_IO.File_Size;
      End_Point   : File_IO.File_Size;
      Size        : File_IO.File_Size;
      Offset      : File_IO.File_Size;
      Track       : Stream_Track;
      State       : Stream_State := Unused;
      Looping     : Boolean;
      FD          : File_IO.File_Descriptor;
   end record;

   Streams : array (Valid_Stream_ID) of Stream_Info;

end WNM.Sample_Stream;
