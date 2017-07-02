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

with Ada.Interrupts;
with STM32.DMA;      use STM32.DMA;
with System;         use System;
with HAL;            use HAL;

package Double_Buffers_Interrupts is

   type Buffer_In_Use is (None, Buffer_0, Buffer_1);
   type Buffer_State is (Empty, Loaded);
   type Buffer_State_Array is array (Memory_Buffer_Target) of Buffer_State;

   protected type DMA_Interrupt_Controller
     (Controller : not null access DMA_Controller;
      Stream     : DMA_Stream_Selector;
      ID         : Ada.Interrupts.Interrupt_ID)
   is
      pragma Interrupt_Priority (Any_Priority'Last);

      procedure Start
        (Destination : Address;
         Source      : Address;
         Data_Count  : UInt16);

      entry Set_Next_Buffer (Source      : Address;
                             Data_Count  : UInt16);

   private

      procedure Interrupt_Handler;
      pragma Attach_Handler (Interrupt_Handler, ID);

      Not_Fully_Loaded : Boolean := True;
      In_Use           : Buffer_In_Use := None;
      State            : Buffer_State_Array := (others => Empty);

      Last_Status             : DMA_Error_Code := DMA_No_Error;
   end DMA_Interrupt_Controller;

   type DMA_Interrupt_Controller_Access is access all DMA_Interrupt_Controller;

end Double_Buffers_Interrupts;
