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

with HAL.Bitmap;
with Giza.Bitmaps.Indexed_1bit;

package WNM.Screen is

   procedure Update;

   function Buffer return not null HAL.Bitmap.Any_Bitmap_Buffer;

   procedure Copy_Bitmap (Bmp          : Giza.Bitmaps.Indexed_1bit.Bitmap_Indexed;
                          X, Y         : Integer;
                          Invert_Color : Boolean := False);

   procedure Sleep;
   procedure Wakeup;

private

   type Screen_Mode is (Text, Parameter);

end WNM.Screen;
