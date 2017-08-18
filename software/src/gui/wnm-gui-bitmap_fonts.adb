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
      Invert_From : Integer := 95;
      Invert_To   : Integer := 95)
   is
      Index : constant Integer := Character'Pos (C) - Character'Pos ('!');
      Bitmap_Offset : constant Integer := Index * 5;

      function Color (X, Y : Integer) return Boolean;
      function Invert (X : Integer) return Boolean;

      -----------
      -- Color --
      -----------

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

      ------------
      -- Invert --
      ------------

      function Invert (X : Integer) return Boolean
      is (X in Invert_From .. Invert_To);

   begin
      Draw_Loop : for X in 0 .. 5 loop
         for Y in 0 .. 7 loop

            if X + X_Offset > Buffer.Width - 1 then
               exit Draw_Loop;
            elsif X + X_Offset >= 0
              and then
                Y + Y_Offset in 0 .. Buffer.Height - 1
            then
               Buffer.Set_Pixel ((X + X_Offset, Y + Y_Offset),
                                 (if Color (X, Y) xor Invert (X + X_Offset)
                                  then White else Black));
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
      Invert_From : Integer := 95;
      Invert_To   : Integer := 95)
   is
      Start : Integer;
   begin
      for C of Str loop
         if X_Offset > Buffer.Width then
            return;
         end if;
         Print (Buffer,
                X_Offset,
                Y_Offset,
                C,
                Invert_From,
                Invert_To);
      end loop;

      if X_Offset < Invert_To then
         Start :=  Integer'Max (X_Offset, Invert_From);

         Buffer.Set_Source (White);
         Buffer.Fill_Rect ((Position => (Start, Y_Offset),
                            Width    => Invert_To - Start,
                            Height   => 8));
      end if;
   end Print;

end WNM.GUI.Bitmap_Fonts;
