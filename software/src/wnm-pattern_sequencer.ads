package WNM.Pattern_Sequencer is

   procedure Add_To_Sequence (Pattern : Patterns);
   procedure End_Sequence_Edit;

   function Current_Pattern return Patterns;
   procedure Signal_End_Of_Pattern;

end WNM.Pattern_Sequencer;
