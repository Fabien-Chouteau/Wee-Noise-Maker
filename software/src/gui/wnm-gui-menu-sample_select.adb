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

with HAL.Bitmap;           use HAL.Bitmap;
with WNM.GUI.Menu.Drawing; use WNM.GUI.Menu.Drawing;
with WNM.Synth;
with WNM.Sequencer;

package body WNM.GUI.Menu.Sample_Select is

   Sample_Select : aliased Sample_Select_Window;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Sample_Select'Access);
   end Push_Window;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (This : in out Sample_Select_Window)
   is
   begin


      if This.From = 0
        or else
         This.To = 0
        or else
          This.From > This.To
      then
         Draw_Menu_Box (Text   => "No samples...",
                        Top    => False,
                        Bottom => False);
         return;
      end if;

      Draw_Menu_Box (Text   => Entry_Name (This.Index),
                     Top    => This.Index /= This.From,
                     Bottom => This.Index /= This.To);
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding procedure On_Event
     (This  : in out Sample_Select_Window;
      Event : Menu_Event)
   is
   begin
      case Event.Kind is
         when Left_Press =>
            Exit_Menu;
         when Right_Press =>
            Menu.Pop (Exit_Value => None);
         when Encoder_Right =>
            null;
         when Encoder_Left =>
            if This.Index /= Invalid_Sample_Entry then
               if Event.Value > 0 then
                  if This.Index /= This.To then
                     This.Index := This.Index + 1;
                  end if;
               elsif Event.Value < 0 then
                  if This.Index /= This.From then
                     This.Index := This.Index - 1;
                  end if;
               end if;
               WNM.Synth.Assign_Sample
                 (WNM.Sequencer.Track,
                  Sample_Library.Entry_Path (This.Index));
            end if;
      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding procedure On_Pushed
     (This  : in out Sample_Select_Window)
   is
   begin
      This.From := Sample_Library.First_Valid_Entry;
      This.To := Sample_Library.Last_Valid_Entry;
      This.Index := This.From;
      if This.Index /= Invalid_Sample_Entry then
         WNM.Synth.Assign_Sample (WNM.Sequencer.Track,
                                    Sample_Library.Entry_Path (This.Index));
      end if;
   end On_Pushed;

   --------------
   -- On_Focus --
   --------------

   overriding procedure On_Focus
     (This       : in out Sample_Select_Window;
      Exit_Value : Window_Exit_Value)
   is
   begin
      null;
   end On_Focus;

end WNM.GUI.Menu.Sample_Select;
