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

package body WNM.Pattern_Sequencer is
   Max_Patterns_In_Sequence : constant := 30;

   subtype Sequence_Range is Positive range 1 .. Max_Patterns_In_Sequence;

   Sequence_Of_Pattern : array (Sequence_Range) of Patterns
     := (others => B1);

   Is_In_Sequence : array (Patterns) of Boolean := (B1     => True,
                                                    others => False);
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

         --  Starting a new sequence
         Edit_In_Progress := True;
         Next_In := Sequence_Range'First;
         Is_In_Sequence := (others => False);
      end if;

      if Next_In in Sequence_Range'Range then
         Sequence_Of_Pattern (Sequence_Range (Next_In)) := Pattern;
         Is_In_Sequence (Pattern) := True;
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

   ----------------------------
   -- Is_In_Pattern_Sequence --
   ----------------------------

   function Is_In_Pattern_Sequence (Pattern : Patterns) return Boolean
     is (Is_In_Sequence (Pattern));

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
