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

with Ada.Text_IO; use Ada.Text_IO;
with Bitmap_Color_Conversion;
with Giza.Bitmaps.Indexed_1bit;
with Giza.Colors;

package body console is

   Screen : aliased Bitmap_Buffer;

   function Image (Int : Integer) return String with Inline_Always;

   -----------
   -- Image --
   -----------

   function Image (Int : Integer) return String is
      Str : constant String := Int'Img;
   begin
      return Str (Str'First + 1 .. Str'Last);
   end Image;


   ------------
   -- Buffer --
   ------------

   function Buffer return not null HAL.Bitmap.Any_Bitmap_Buffer
   is (Screen'Access);

   -------------------
   -- Update_Screen --
   -------------------

   procedure Update_Screen is
      procedure Put_C (X, Y : Integer; C : Character);

      -----------
      -- Put_C --
      -----------

      procedure Put_C (X, Y : Integer; C : Character) is
      begin
         Put (ASCII.ESC & "[" & Image (Y) & ";" & Image (X) & "H");
         Put (C);
      end Put_C;
   begin
      Put (ASCII.ESC & "[2J");

      --  Draw top and bottom boarders
      for X in 0 .. Screen.Screen'Length (1) + 2 loop
         Put_C (X, 0, '-');
         Put_C (X, Screen.Screen'Length (2) + 2, '-');
      end loop;

      --  Draw right and left boarders
      for Y in 0 .. Screen.Screen'Length (2) + 2 loop
         Put_C (0, Y, '|');
         Put_C (Screen.Screen'Length (1) + 2, Y, '|');
      end loop;

      --  Draw pixels
      for X in Screen.Screen'Range (1) loop
         for Y in Screen.Screen'Range (2) loop
            Put_C (X + 2, Y + 2, (if Screen.Screen (X, Y) then '#' else ' '));
         end loop;
      end loop;
   end Update_Screen;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel
     (Buffer  : in out Bitmap_Buffer;
      Pt      : HAL.Bitmap.Point;
      Value   : HAL.Bitmap.Bitmap_Color)
     is
   begin
      Buffer.Set_Pixel
        (Pt,
         Bitmap_Color_Conversion.Bitmap_Color_To_Word (HAL.Bitmap.M_1,Value));
   end Set_Pixel;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel
     (Buffer  : in out Bitmap_Buffer;
      Pt      : HAL.Bitmap.Point;
      Value   : HAL.UInt32)
   is
      use type HAL.UInt32;
   begin
      if Pt.X in Buffer.Screen'Range (1)
        and then
         Pt.Y in Buffer.Screen'Range (2)
      then
         Buffer.Screen (Pt.X, Pt.Y) := Value /= 0;
      end if;
   end Set_Pixel;

   ---------------------
   -- Set_Pixel_Blend --
   ---------------------

   procedure Set_Pixel_Blend
     (Buffer : in out Bitmap_Buffer;
      Pt     : HAL.Bitmap.Point;
      Value  : HAL.Bitmap.Bitmap_Color)
   is
   begin
      Buffer.Set_Pixel
        (Pt,
         Bitmap_Color_Conversion.Bitmap_Color_To_Word (HAL.Bitmap.M_1,Value));
   end Set_Pixel_Blend;

   -----------
   -- Pixel --
   -----------

   function Pixel
     (Buffer : Bitmap_Buffer;
      Pt     : HAL.Bitmap.Point)
      return HAL.Bitmap.Bitmap_Color
   is
   begin
      return (if Buffer.Screen (Pt.X, Pt.Y)
              then HAL.Bitmap.White
              else HAL.Bitmap.Black);
   end Pixel;

   -----------
   -- Pixel --
   -----------

   function Pixel
     (Buffer : Bitmap_Buffer;
      Pt     : HAL.Bitmap.Point)
      return HAL.UInt32
   is
   begin
      return Bitmap_Color_Conversion.Bitmap_Color_To_Word (HAL.Bitmap.M_1,
                                                           Buffer.Pixel (Pt));
   end Pixel;


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
            Screen.Set_Pixel ((X + W, Y + H),
                              (if Giza_Color.R /= 0 then
                                  HAL.Bitmap.White
                               else
                                  HAL.Bitmap.Black));
         end loop;
      end loop;

   end Copy_Bitmap;


   ----------------
   -- Set_Source --
   ----------------

   procedure Set_Source (Buffer : in out Bitmap_Buffer;
                         ARGB   : Bitmap_Color)
   is
   begin
      Buffer.Native_Color := Bitmap_Color_Conversion.Bitmap_Color_To_Word
        (Buffer.Color_Mode, ARGB);
   end Set_Source;

   ----------------
   -- Set_Source --
   ----------------

   procedure Set_Source (Buffer : in out Bitmap_Buffer;
                         Native : UInt32)
   is
   begin
      Buffer.Native_Color := Native;
   end Set_Source;

   ------------
   -- Source --
   ------------

   function Source
     (Buffer : Bitmap_Buffer)
      return Bitmap_Color
   is
   begin
      return Bitmap_Color_Conversion.Word_To_Bitmap_Color (Buffer.Color_Mode, Buffer.Native_Color);
   end Source;

   ------------
   -- Source --
   ------------

   function Source
     (Buffer : Bitmap_Buffer)
      return UInt32
   is
   begin
      return Buffer.Native_Color;
   end Source;

   ---------------------
   -- Set_Pixel_Blend --
   ---------------------

   procedure Set_Pixel_Blend
     (Buffer : in out Bitmap_Buffer;
      Pt     : Point)
   is
   begin
      raise Program_Error;
   end Set_Pixel_Blend;

   ---------------
   -- Set_Pixel --
   ---------------

   overriding
   procedure Set_Pixel
     (Buffer  : in out Bitmap_Buffer;
      Pt      : HAL.Bitmap.Point)
   is
   begin
      Buffer.Set_Pixel (Pt, Buffer.Native_Color);
   end Set_Pixel;

end Console;
