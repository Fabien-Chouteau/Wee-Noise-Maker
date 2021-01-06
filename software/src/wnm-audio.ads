with Quick_Synth;

package WNM.Audio is

   type Input_Kind is (Line_In, FM);
   procedure Select_Input (Kind : Input_Kind);

   type DAC_Volume is range 0 .. 100;
   procedure Set_Volume (Volume : DAC_Volume);


   generic
      with procedure Process (Out_L, Out_R : out Quick_Synth.Mono_Buffer;
                              In_L,  In_R  :     Quick_Synth.Mono_Buffer);
   procedure Generate_Audio;

end WNM.Audio;
