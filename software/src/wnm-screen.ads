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

package WNM.Screen is

   procedure Initialize;

   procedure Update;

   function Buffer return not null HAL.Bitmap.Any_Bitmap_Buffer;

   procedure Sleep;
   procedure Wakeup;

   procedure Text (Str : String);

--     procedure Splash (Str      : String;
--                       Duration : Time_Span);
--
--     procedure Parameter (Str : String);
--     procedure Parameter_Value (Val : Integer);

private

   type Screen_Mode is (Text, Parameter);

end WNM.Screen;
