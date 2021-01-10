-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                  Copyright (C) 2016-2021 Fabien Chouteau                  --
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

with SDL.Events;           use SDL.Events;
with SDL.Events.Events;    use SDL.Events.Events;
with SDL.Events.Keyboards; use SDL.Events.Keyboards;

with GNAT.OS_Lib;
with SDL.Events.Mice;

with Interfaces; use Interfaces;

package body WNM.Buttons is

   Key_State : array (Button) of Raw_Button_State := (others => Up);
   Encoder_Right, Encoder_Left : Integer := 0;

   procedure Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      null;
   end Initialize;

   ----------
   -- Scan --
   ----------

   procedure Scan is
      Event   : SDL.Events.Events.Events;
      State   : Raw_Button_State;
   begin
      Encoder_Right := 0;
      Encoder_Left := 0;

      pragma Warnings (Off, "infinite loop");
      while SDL.Events.Events.Poll (Event) loop

         case Event.Common.Event_Type is
            when Key_Down | Key_Up =>
               if Event.Keyboard.Repeat = 0 then
                  State := (if Event.Common.Event_Type = Key_Down
                            then Down
                            else Up);

                  case Event.Keyboard.Key_Sym.Scan_Code is

                  when Scan_Code_Return =>
                     Key_State (Play) := State;
                  when Scan_Code_Right_Shift =>
                     Key_State (Rec) := State;

                  when Scan_Code_W =>
                     Key_State (B1) := State;
                  when Scan_Code_E =>
                     Key_State (B2) := State;
                  when Scan_Code_R =>
                     Key_State (B3) := State;
                  when Scan_Code_T =>
                     Key_State (B4) := State;
                  when Scan_Code_Y =>
                     Key_State (B5) := State;
                  when Scan_Code_U =>
                     Key_State (B6) := State;
                  when Scan_Code_I =>
                     Key_State (B7) := State;
                  when Scan_Code_O =>
                     Key_State (B8) := State;
                  when Scan_Code_S =>
                     Key_State (B9) := State;
                  when Scan_Code_D =>
                  Key_State (B10) := State;
                  when Scan_Code_F =>
                     Key_State (B11) := State;
                  when Scan_Code_G =>
                     Key_State (B12) := State;
                  when Scan_Code_H =>
                     Key_State (B13) := State;
                  when Scan_Code_J =>
                     Key_State (B14) := State;
                  when Scan_Code_K =>
                     Key_State (B15) := State;
                  when Scan_Code_L =>
                     Key_State (B16) := State;

                  when Scan_Code_A =>
                     Key_State (Pattern) := State;
                  when Scan_Code_Q =>
                     Key_State (Track_Button) := State;

                  when Scan_Code_1 =>
                     Key_State (Encoder_L) := State;
                  when Scan_Code_2 =>
                     Key_State (Encoder_R) := State;
                  when Scan_Code_3 =>
                     Key_State (Menu) := State;
                  when Scan_Code_4 =>
                     Key_State (Func) := State;

                  when Scan_Code_Up =>
                     if State = Down then
                        Encoder_Left := Encoder_Left - 1;
                     end if;
                  when Scan_Code_Down =>
                     if State = Down then
                        Encoder_Left := Encoder_Left + 1;
                     end if;
                  when Scan_Code_Left =>
                     if State = Down then
                        Encoder_Right := Encoder_Right - 1;
                     end if;
                  when Scan_Code_Right =>
                     if State = Down then
                        Encoder_Right := Encoder_Right + 1;
                     end if;

                  when Scan_Code_Escape =>
                     GNAT.OS_Lib.OS_Exit (0);
                  when others =>
                     null;
                  end case;
               end if;
            when SDL.Events.Mice.Wheel =>
               Encoder_Left := Integer (Event.Mouse_Wheel.X);
               Encoder_Right := Integer (Event.Mouse_Wheel.Y);
            when others =>
               null;
         end case;
      end loop;
   end Scan;

   -----------
   -- State --
   -----------

   function State (B : Button) return Raw_Button_State
   is (Key_State (B));

   ----------------
   -- Is_Pressed --
   ----------------

   function Is_Pressed (B : Button) return Boolean
   is (Key_State (B) = Down);


   ----------
   -- Left --
   ----------

   function Left_Diff return Integer is
   begin
      return Encoder_Left;
   end Left_Diff;

   -----------
   -- Right --
   -----------

   function Right_Diff return Integer is
   begin
      return Encoder_Right;
   end Right_Diff;

begin
   Initialize;
end WNM.Buttons;
