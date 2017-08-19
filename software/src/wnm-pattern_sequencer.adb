package body WNM.Pattern_Sequencer is
   Max_Patterns_In_Sequence : constant := 30;

   subtype Sequence_Range is Positive range 1 .. Max_Patterns_In_Sequence;

   Sequence_Of_Pattern : array (Sequence_Range) of Patterns
     := (others => B1);

   Current_Pattern_Index : Sequence_Range := 1;
   Last_In_Sequence      : Sequence_Range := 1;
   Next_In               : Positive := 1;
   Edit_In_Progress      : Boolean := False;

   ---------------------
   -- Add_To_Sequence --
   ---------------------

   procedure Add_To_Sequence (Pattern : Patterns) is
   begin
      if not Edit_In_Progress then
         Edit_In_Progress := True;
         Next_In := Sequence_Range'First;
      end if;

      if Next_In in Sequence_Range'Range then
         Sequence_Of_Pattern (Sequence_Range (Next_In)) := Pattern;
         Last_In_Sequence := Next_In;
         Next_In := Next_In + 1;
      end if;
   end Add_To_Sequence;

   -----------------------
   -- End_Sequence_Edit --
   -----------------------

   procedure End_Sequence_Edit is
   begin
      Edit_In_Progress := False;
   end End_Sequence_Edit;

   ---------------------
   -- Current_Pattern --
   ---------------------

   function Current_Pattern return Patterns is
   begin
      return Sequence_Of_Pattern (Current_Pattern_Index);
   end Current_Pattern;

   ---------------------------
   -- Signal_End_Of_Pattern --
   ---------------------------

   procedure Signal_End_Of_Pattern is
   begin
      if Current_Pattern_Index >= Last_In_Sequence then
         Current_Pattern_Index := Sequence_Range'First;
      else
         Current_Pattern_Index := Current_Pattern_Index + 1;
      end if;
   end Signal_End_Of_Pattern;

end WNM.Pattern_Sequencer;
