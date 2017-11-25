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

package body WNM.LED is

   procedure Turn_On (LED_Addr : LED_Address) with Inline_Always;
   procedure Turn_Off (LED_Addr : LED_Address) with Inline_Always;

   -----------
   -- Start --
   -----------

   procedure Start is
      Config : GPIO_Port_Configuration;
   begin
      -- LEDs --

      Config.Mode := Mode_Out;
      Config.Output_Type := Push_Pull;
      Config.Resistors := Floating;

      for Pt of Row_To_Point loop
         Enable_Clock (Pt);
         Pt.Configure_IO (Config);
         Pt.Set;
      end loop;

      for Pt of Col_To_Point loop
         Enable_Clock (Pt);
         Pt.Configure_IO (Config);
         Pt.Clear;
      end loop;

      for B in LEDs loop
         Turn_Off (B);
      end loop;

      --  LED Timer --

      Enable_Clock (LED_Timer);

      Reset (LED_Timer);

      Configure (LED_Timer, Prescaler => 100, Period => 100);

      Enable_Interrupt (LED_Timer, Timer_Update_Interrupt);

      Enable (LED_Timer);
   end Start;

   -------------
   -- Turn_On --
   -------------

   procedure Turn_On (B : LEDs) is
   begin
      LED_State (B) := LED_State (B) + 1;
   end Turn_On;

   --------------
   -- Turn_Off --
   --------------

   procedure Turn_Off (B : LEDs) is
   begin
      LED_State (B) := 0;
   end Turn_Off;

   ------------------
   -- Turn_Off_All --
   ------------------

   procedure Turn_Off_All is
   begin
      LED_State := (others => 0);
   end Turn_Off_All;
   -------------
   -- Turn_On --
   -------------

   procedure Turn_On (LED_Addr : LED_Address)
   is
   begin
      Row_To_Point (LED_Addr.Row).Set;
      Col_To_Point (LED_Addr.Col).Clear;
   end Turn_On;

   --------------
   -- Turn_Off --
   --------------

   procedure Turn_Off (LED_Addr : LED_Address)
   is
   begin
      Row_To_Point (LED_Addr.Row).Clear;
      Col_To_Point (LED_Addr.Col).Set;
   end Turn_Off;

   -----------------------
   -- LED_Timer_Handler --
   -----------------------

   protected body LED_Timer_Handler is

      -----------------
      -- IRQ_Handler --
      -----------------

      procedure IRQ_Handler is
      begin
         if Status (LED_Timer, Timer_Update_Indicated) then
            if Interrupt_Enabled (LED_Timer, Timer_Update_Interrupt) then
               Clear_Pending_Interrupt (LED_Timer, Timer_Update_Interrupt);
            end if;

            Turn_Off (LED_To_Address (Current_LED));

            if Current_LED = LEDs'Last then
               Current_LED := LEDs'First;
            else
               Current_LED := LEDs'Succ (Current_LED);
            end if;

            if LED_State (Current_LED) > 0 then
               Turn_On (LED_To_Address (Current_LED));
               Configure (LED_Timer,
                          Prescaler => 50,
                          Period => (case LED_State (Current_LED) is
                                        when 1      => 700,
                                        when others => 3000));
            else
               Configure (LED_Timer,
                          Prescaler => 50,
                          Period => 500);
            end if;

         end if;
      end IRQ_Handler;
   end LED_Timer_Handler;

end WNM.LED;
