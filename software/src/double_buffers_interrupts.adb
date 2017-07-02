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

package body Double_Buffers_Interrupts is

   ------------------------------
   -- DMA_Interrupt_Controller --
   ------------------------------

   protected body DMA_Interrupt_Controller is

      -----------
      -- Start --
      -----------

      procedure Start
        (Destination : Address;
         Source      : Address;
         Data_Count  : UInt16)
      is
      begin

         Configure_Data_Flow
           (Controller.all,
            Stream,
            Source      => Source,
            Destination => Destination,
            Data_Count  => Data_Count);

         for Selected_Interrupt in DMA_Interrupt loop
            Enable_Interrupt (Controller.all, Stream, Selected_Interrupt);
         end loop;


         Configure_Double_Buffered_Mode (This              => Controller.all,
                                         Stream            => Stream,
                                         Buffer_0_Value    => Source,
                                         Buffer_1_Value    => Source,
                                         First_Buffer_Used => Memory_Buffer_0);

         Not_Fully_Loaded := True;
         In_Use := Buffer_0;
         State (Memory_Buffer_0) := Loaded;
         State (Memory_Buffer_1) := Empty;

         Clear_All_Status (Controller.all, Stream);

         Enable_Double_Buffered_Mode (Controller.all, Stream);

         Enable (Controller.all, Stream);
      end Start;

      ---------------------
      -- Set_Next_Buffer --
      ---------------------

      entry Set_Next_Buffer
        (Source      : Address;
         Data_Count  : UInt16)
         when Not_Fully_Loaded
      is
      begin
         if Data_Count /= Current_NDT (Controller.all, Stream) then
            null;
         end if;

         case In_Use is
            when None =>
               raise Program_Error with "TODO...";
            when Buffer_0 =>
               Set_Memory_Buffer (Controller.all,
                                  Stream,
                                  Memory_Buffer_1,
                                  Source);
               State (Memory_Buffer_1) := Loaded;
            when Buffer_1 =>
               Set_Memory_Buffer (Controller.all,
                                  Stream,
                                  Memory_Buffer_0,
                                  Source);
               State (Memory_Buffer_0) := Loaded;
         end case;

         Not_Fully_Loaded :=
           State (Memory_Buffer_0) = Empty
           or else
           State (Memory_Buffer_1) = Empty;
      end Set_Next_Buffer;

      -----------------------
      -- Interrupt_Handler --
      -----------------------

      procedure Interrupt_Handler is
      begin
         for Flag in DMA_Status_Flag loop
            if Status (Controller.all, Stream, Flag) then
               case Flag is
                  when FIFO_Error_Indicated =>
                     Last_Status := DMA_FIFO_Error;
                  when Direct_Mode_Error_Indicated =>
                     Last_Status := DMA_Direct_Mode_Error;
                  when Transfer_Error_Indicated =>
                     Last_Status := DMA_Transfer_Error;
                  when Half_Transfer_Complete_Indicated =>
                     null;
                  when Transfer_Complete_Indicated =>
                     Not_Fully_Loaded := True;

                     case In_Use is
                     when None =>
                        raise Program_Error with "Whut?!?";
                     when Buffer_0 =>
                        In_Use := Buffer_1;
                        State (Memory_Buffer_0) := Empty;

                     when Buffer_1 =>
                        In_Use := Buffer_0;
                        State (Memory_Buffer_1) := Empty;
                     end case;
               end case;
               Clear_Status (Controller.all, Stream, Flag);
            end if;
         end loop;

      end Interrupt_Handler;

   end DMA_Interrupt_Controller;

end Double_Buffers_Interrupts;
