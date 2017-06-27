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

--  This package provide emulation of a monochrome screen on a text console.

with HAL.Bitmap; use HAL.Bitmap;
with Giza.Bitmaps.Indexed_1bit;

private with Soft_Drawing_Bitmap;
with System; use System;
with HAL;    use HAL;

package Console is

   function Buffer return not null HAL.Bitmap.Any_Bitmap_Buffer;

   procedure Update_Screen;

   procedure Copy_Bitmap (Bmp  : Giza.Bitmaps.Indexed_1bit.Bitmap_Indexed;
                          X, Y : Integer);

private

   type Screen_Array is array (0 .. 95, 0 .. 15) of Boolean;

   type Bitmap_Buffer is new Soft_Drawing_Bitmap.Soft_Drawing_Bitmap_Buffer
     with record
      Screen : Screen_Array := (others => (others => False));
      Native_Color : UInt32;
   end record;

   overriding
   procedure Set_Source (Buffer : in out Bitmap_Buffer;
                         ARGB   : Bitmap_Color);

   overriding
   procedure Set_Source (Buffer : in out Bitmap_Buffer;
                         Native : UInt32);

   overriding
   function Source
     (Buffer : Bitmap_Buffer)
      return Bitmap_Color;

   overriding
   function Source
     (Buffer : Bitmap_Buffer)
      return UInt32;

   function Width (Buffer : Bitmap_Buffer) return Natural is (96);

   function Height (Buffer : Bitmap_Buffer) return Natural is (16);

   function Swapped (Buffer : Bitmap_Buffer) return Boolean is (False);

   function Color_Mode (Buffer : Bitmap_Buffer) return HAL.Bitmap.Bitmap_Color_Mode is
     (HAL.Bitmap.M_1);

   function Mapped_In_RAM (Buffer : Bitmap_Buffer) return Boolean is
     (False);

   function Memory_Address (Buffer : Bitmap_Buffer) return System.Address is
     (System.Null_Address);

   overriding
   procedure Set_Pixel
     (Buffer  : in out Bitmap_Buffer;
      Pt      : HAL.Bitmap.Point);

   procedure Set_Pixel
     (Buffer  : in out Bitmap_Buffer;
      Pt      : HAL.Bitmap.Point;
      Value   : HAL.Bitmap.Bitmap_Color);

   procedure Set_Pixel
     (Buffer  : in out Bitmap_Buffer;
      Pt      : HAL.Bitmap.Point;
      Value   : HAL.UInt32);

   procedure Set_Pixel_Blend
     (Buffer : in out Bitmap_Buffer;
      Pt     : HAL.Bitmap.Point;
      Value  : HAL.Bitmap.Bitmap_Color);

   procedure Set_Pixel_Blend
     (Buffer : in out Bitmap_Buffer;
      Pt     : Point);

   function Pixel
     (Buffer : Bitmap_Buffer;
      Pt     : HAL.Bitmap.Point)
      return HAL.Bitmap.Bitmap_Color;

   function Pixel
     (Buffer : Bitmap_Buffer;
      Pt     : HAL.Bitmap.Point)
      return HAL.UInt32;

   function Buffer_Size (Buffer : Bitmap_Buffer) return Natural is (96 * 16);

end Console;
