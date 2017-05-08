
with Console;

with wnm_logo_text;
with wnm_logo_wave_1;
with wnm_logo_wave_2;
with wnm_logo_wave_3;
with wnm_logo_wave_4;

with HAL.Bitmap; use HAL.Bitmap;
with WNM.GUI.Bitmap_Fonts;

procedure Main_Screen_Test is
   A : Integer := 0;
begin

   for I in 1 .. 1_000_000 loop
      Console.Buffer.Fill (Black);
--        Console.Copy_Bitmap (Bmp => wnm_logo_text.Data,
--                             X   => 6,
--                             Y   => 1);
--        Console.Buffer.Draw_Line (Color     => White,
--                                  Start     => (6, 10),
--                                  Stop      => (32, 10),
--                                  Thickness => 1);
--        Console.Buffer.Draw_Line (Color     => White,
--                                  Start     => (63, 10),
--                                  Stop      => (89, 10),
--                                  Thickness => 1);
--        Console.Copy_Bitmap ((case (I mod 4) is
--                                when 0 => wnm_logo_wave_1.Data,
--                                when 1 => wnm_logo_wave_2.Data,
--                                when 2 => wnm_logo_wave_3.Data,
--                                when others => wnm_logo_wave_4.Data),
--                             33, 8);

      A :=  -20;
      for C of String'("Volume: " & Integer'Image (I mod 96) & "%       ") loop
         WNM.GUI.Bitmap_Fonts.Print_Glyph (Buffer      => Console.Buffer.all,
                                           X_Offset    => A,
                                           Y_Offset    => 0,
                                           C           => C,
                                           Invert_From => I mod 96);
      end loop;
      A := 0;
      for C of String'("""#:;%&*+-_.94=>?") loop
         WNM.GUI.Bitmap_Fonts.Print_Glyph (Buffer   => Console.Buffer.all,
                                           X_Offset => A,
                                           Y_Offset => 8,
                                           C        => C,
                                           Invert_From => I mod 96);
      end loop;
      Console.Update_Screen;
      delay 1.0 / 10.0;
   end loop;
end Main_Screen_Test;
