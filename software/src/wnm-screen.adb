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

with STM32.Device;                 use STM32.Device;
with STM32.GPIO;                   use STM32.GPIO;
with HAL;                          use HAL;
with HAL.GPIO;                     use HAL.GPIO;
with SSD1306.Standard_Resolutions; use SSD1306.Standard_Resolutions;
with Ravenscar_Time;
with HAL.Bitmap;                   use HAL.Bitmap;
with Ada.Unchecked_Deallocation;

with WNM.I2C;

with wnm_logo_wave_1;
with wnm_logo_wave_2;
with wnm_logo_wave_3;
with wnm_logo_wave_4;
with wnm_logo_text;

with Giza.Bitmaps.Indexed_1bit;
with Giza.Colors;

package body WNM.Screen is

   type String_Access is access all String;

--     Current_Mode : Screen_Mode := Text;
   Current_Text : String_Access := new String'("Wee Noise Maker");

   Screen_Reset : STM32.GPIO.GPIO_Point renames PC13;

   Screen : SSD1306_96x16_Screen (WNM.I2C.Port,
                                  Screen_Reset'Access,
                                  Ravenscar_Time.Delays);

   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

   procedure Copy_Bitmap (Bmp  : Giza.Bitmaps.Indexed_1bit.Bitmap_Indexed;
                          X, Y : Integer);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Unref : Boolean with Unreferenced;

   begin
      Enable_Clock (Screen_Reset);
      Unref := Screen_Reset.Set_Mode (Output);

      Screen.Initialize (External_VCC => False);
      Screen.Initialize_Layer (1, M_1);

      Wakeup;
   end Initialize;

   -----------------
   -- Copy_Bitmap --
   -----------------

   procedure Copy_Bitmap (Bmp  : Giza.Bitmaps.Indexed_1bit.Bitmap_Indexed;
                          X, Y : Integer)
   is
      use type Giza.Colors.RGB_Component;

      Giza_Color : Giza.Colors.Color;
   begin
      for W in 0 .. Bmp.W - 1 loop
         for H in 0 .. Bmp.H - 1 loop
            Giza_Color := Giza.Bitmaps.Indexed_1bit.Get_Pixel (Bmp, (W, H));
            Screen.Hidden_Buffer (1).Set_Pixel ((X + W, Y + H),
                                                (if Giza_Color.R /= 0 then
                                                    White
                                                 else
                                                    Black));
         end loop;
      end loop;

   end Copy_Bitmap;

   -----------
   -- Sleep --
   -----------

   procedure Sleep is
   begin
      Screen.Turn_Off;
   end Sleep;

   ------------
   -- Wakeup --
   ------------

   procedure Wakeup is
   begin
      Screen.Turn_On;

      Copy_Bitmap (wnm_logo_text.Data, 6, 1);
      Screen.Hidden_Buffer (1).Set_Source (White);
      Screen.Hidden_Buffer (1).Draw_Line (Start     => (6, 10),
                                          Stop      => (32, 10),
                                          Thickness => 1);
      Screen.Hidden_Buffer (1).Draw_Line (Start     => (63, 10),
                                          Stop      => (89, 10),
                                          Thickness => 1);

      for I in 1 .. 5 loop
         Copy_Bitmap ((case (I mod 4) is
                         when 0 => wnm_logo_wave_1.Data,
                         when 1 => wnm_logo_wave_2.Data,
                         when 2 => wnm_logo_wave_3.Data,
                         when others => wnm_logo_wave_4.Data),
                      33, 8);
         Screen.Update_Layers;
         delay until Clock + Milliseconds (200);
      end loop;
   end Wakeup;

   ------------
   -- Update --
   ------------

   procedure Update is
   begin
      Screen.Update_Layers;
   end Update;

   ------------
   -- Buffer --
   ------------

   function Buffer return not null HAL.Bitmap.Any_Bitmap_Buffer
   is (Screen.Hidden_Buffer (1));

   ----------
   -- Text --
   ----------

   procedure Text (Str : String) is
   begin
      if Current_Text /= null then
         Free (Current_Text);
      end if;

      Current_Text := new String'(Str);
--        Current_Mode := Text;

   end Text;
end WNM.Screen;
