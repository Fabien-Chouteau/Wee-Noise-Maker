
with WNM.Sequencer;
with WNM.UI;
with WNM.GUI.Update;
with WNM.Master_Volume;
with WNM.Sample_Library;
with WNM.Audio;
with Quick_Synth;

procedure Wee_Noise_Maker_Sim is
begin


   WNM.Sample_Library.Load;

   Quick_Synth.Load_Samples;

   WNM.UI.Start;
   WNM.Sequencer.Start;

   loop
      Quick_Synth.Update;
      WNM.Audio.Set_Volume (50);
      WNM.Master_Volume.Update;
      WNM.GUI.Update.Update;
      --  delay 0.1;
   end loop;


   --  loop
   --     WNM.GUI.Update.Update;
   --     WNM.Buttons.Scan;
   --     WNM.Screen.Update;
   --     WNM.Screen.Copy_Bitmap (Bmp          => (W => 84, H => 5, Length_Byte => 53,
   --                                              Data => (222, 48, 156, 27, 131, 97, 184, 55, 118, 67, 232, 245, 253, 181, 238, 238, 251, 51, 93, 219, 119, 82, 24, 222, 234,
   --                                                       238, 33, 188, 20, 196, 97, 216, 246, 253, 157, 238, 254, 250, 123, 93, 219, 119, 237, 48, 220, 27, 131, 112, 184, 215,
   --                                                       117, 67, 71)),
   --                             X            => 6,
   --                             Y            => 1);
   --
   --     WNM.Screen.Copy_Bitmap (Bmp          => (W => 30, H => 5, Length_Byte => 19,
   --                                               Data => (189, 119, 239, 173, 206, 154, 179, 178, 173, 85, 181, 158, 243, 230, 222, 247, 190, 189, 183)),
   --                             X            => 33,
   --                             Y            => 8);
   --  end loop;
end Wee_Noise_Maker_Sim;
