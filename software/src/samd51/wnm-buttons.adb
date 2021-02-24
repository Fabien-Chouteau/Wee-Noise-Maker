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

with HAL.GPIO;

with SAM.Port;
with SAM.Device;

with WNM.Samd51.Encoders;
with WNM.Time;

with HAL; use HAL;

package body WNM.Buttons is

   Col_In    : SAM.Port.GPIO_Point renames SAM.Device.PA23; -- Feather D13
   Row_Out   : SAM.Port.GPIO_Point renames SAM.Device.PA22; -- Feather D12
   Row_Latch : SAM.Port.GPIO_Point renames SAM.Device.PA21; -- Feather D11
   Clk       : SAM.Port.GPIO_Point renames SAM.Device.PA20; -- Feather D10

   Key_State : constant array (Button) of Raw_Button_State := (others => Up);

   Rows : UInt8_Array (1 .. 8) := (others => 0);

   procedure Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Col_In.Clear;
      Col_In.Set_Mode (HAL.GPIO.Output);
      Col_In.Set_Pull_Resistor (HAL.GPIO.Floating);

      Row_Out.Clear;
      Row_Out.Set_Mode (HAL.GPIO.Input);
      Row_Out.Set_Pull_Resistor (HAL.GPIO.Floating);

      Row_Latch.Clear;
      Row_Latch.Set_Mode (HAL.GPIO.Output);
      Row_Latch.Set_Pull_Resistor (HAL.GPIO.Floating);

      Clk.Clear;
      Clk.Set_Mode (HAL.GPIO.Output);
      Clk.Set_Pull_Resistor (HAL.GPIO.Floating);
   end Initialize;

   ----------
   -- Scan --
   ----------

   procedure Scan is

      --------------
      -- Do_Clock --
      --------------

      procedure Do_Clock is
      begin
         Clk.Clear;
         WNM.Time.Delay_Ms (1);
         Clk.Set;
         WNM.Time.Delay_Ms (1);
      end Do_Clock;

      -------------
      -- Col_Set --
      -------------

      procedure Col_Set (Val : UInt8) is
         Col : UInt8 := Val;
      begin
         for I in 1 .. 8 loop
            if (Col and 1) /= 0 then
               Col_In.Set;
            else
               Col_In.Clear;
            end if;
            Do_Clock;
            Col := Shift_Right (Col, 1);
         end loop;
      end Col_Set;

      -------------
      -- Row_Get --
      -------------

      procedure Row_Get (Val : out UInt8) is
      begin
         Row_Latch.Clear;
         WNM.Time.Delay_Ms (1);
         Row_Latch.Set;
         Val := 0;
         for I in 1 .. 8 loop
            if Row_Out.Set then
               Val := Val or 1;
            end if;
            Val := Shift_Left (Val, 1);
            Do_Clock;
         end loop;
      end Row_Get;

      Col : UInt8 := 1;
   begin

      if True then
         return;
      end if;

      Col_Set (0);
      WNM.Time.Delay_Ms (1);
      for Elt of Rows loop
         Col_Set (Col);
         Row_Get (Elt);
         Col := Shift_Left (Col, 1);
         WNM.Time.Delay_Ms (5);
      end loop;
      Col_Set (0);

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
      return WNM.Samd51.Encoders.Left;
   end Left_Diff;

   -----------
   -- Right --
   -----------

   function Right_Diff return Integer is
   begin
      return WNM.Samd51.Encoders.Right;
   end Right_Diff;

begin
   Initialize;
end WNM.Buttons;
