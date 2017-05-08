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

with Giza.Bitmaps.Indexed_1bit;
with Giza.Colors;
with HAL.Bitmap;                use HAL.Bitmap;
with font_5x7;

package body WNM.GUI.Bitmap_Fonts is

   -----------
   -- Print --
   -----------

   procedure Print
     (Buffer      : in out HAL.Bitmap.Bitmap_Buffer'Class;
      X_Offset    : in out Integer;
      Y_Offset    : Integer;
      C           : Character;
      Invert_From : Integer := Integer'Last)
   is
      Index : constant Integer := Character'Pos (C) - Character'Pos ('!');
      Bitmap_Offset : constant Integer := Index * 5;

      function Color (X, Y : Integer) return Boolean;

      function Color (X, Y : Integer) return Boolean is
         Giza_Color : Giza.Colors.Color;
         use type Giza.Colors.RGB_Component;
      begin
         if Index in 0 .. 93 and then X in 0 .. 4 and then Y in 1 .. 7 then
            Giza_Color := Giza.Bitmaps.Indexed_1bit.Get_Pixel
              (font_5x7.Data, (X + Bitmap_Offset, Y - 1));
            return Giza_Color.R /= 0;
         else
            return False;
         end if;
      end Color;
   begin
      Draw_Loop : for X in 0 .. 5 loop
         for Y in 0 .. 7 loop

            if X + X_Offset > Buffer.Width - 1 then
               exit Draw_Loop;
            elsif X + X_Offset >= 0 and then Y + Y_Offset in 0 .. Buffer.Height - 1 then
               Buffer.Set_Pixel ((X + X_Offset, Y + Y_Offset),
                                 (if Color (X, Y) then
                                    (if X + X_Offset > Invert_From then White else Black)
                                  else
                                    (if X + X_Offset > Invert_From then Black else White)));
            end if;
         end loop;
      end loop Draw_Loop;

      X_Offset := X_Offset + 6;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Buffer      : in out HAL.Bitmap.Bitmap_Buffer'Class;
      X_Offset    : in out Integer;
      Y_Offset    : Integer;
      Str         : String;
      Invert_From : Integer := Integer'Last)
   is
   begin
      for C of Str loop
         if X_Offset > Buffer.Width then
            return;
         end if;
         Print (Buffer,
                X_Offset,
                Y_Offset,
                C,
                Invert_From);
      end loop;
   end Print;

end WNM.GUI.Bitmap_Fonts;
