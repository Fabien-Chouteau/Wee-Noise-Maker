with WNM.Sequencer;
with WNM.UI;
with WNM.LED;
with WNM.GUI.Update;
with WNM.Master_Volume;
with WNM.Sample_Library;
with WNM.Audio;
with WNM.Synth;
with WNM.Time; use WNM.Time;

with WNM_Sim;
pragma Unreferenced (WNM_Sim);

procedure Wee_Noise_Maker_Sim is
   Next_Start : Time_Ms;

begin

   WNM.Sample_Library.Load;

   WNM.Synth.Load_Samples;

   loop
      Next_Start := Time_Ms'Last;

      Next_Start := Time_Ms'Min (WNM.Synth.Update, Next_Start);
      Next_Start := Time_Ms'Min (WNM.Sequencer.Update, Next_Start);
      Next_Start := Time_Ms'Min (WNM.UI.Update, Next_Start);
      Next_Start := Time_Ms'Min (WNM.LED.Update, Next_Start);
      Next_Start := Time_Ms'Min (WNM.GUI.Update.Update, Next_Start);

      WNM.Audio.Set_Volume (50);
      WNM.Master_Volume.Update;

      Delay_Ms (Next_Start);
   end loop;

end Wee_Noise_Maker_Sim;
