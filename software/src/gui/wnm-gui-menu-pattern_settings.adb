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

package body WNM.GUI.Menu.Pattern_Settings is

   Pattern_Menu_Singleton : aliased Pattern_Settings_Menu;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Pattern_Menu_Singleton'Access);
   end Push_Window;

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw (This : in out Pattern_Settings_Menu)
   is
   begin
      Draw_Menu_Box ("Pattern settings",
                     Count => Settings_Count,
                     Index => Settings'Pos (This.Current_Setting));
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding
   procedure On_Event (This  : in out Pattern_Settings_Menu;
                       Event : Menu_Event)
   is
   begin
      case Event.Kind is
         when Left_Press =>
            null;
         when Right_Press =>
            --  Never exit the step settings
            null;
         when Encoder_Right =>
            null;
         when Encoder_Left =>
            pragma Warnings (Off, "condition can only be True");
            if Event.Value > 0 then
               if This.Current_Setting /= Settings'Last then
                  This.Current_Setting := Settings'Succ (This.Current_Setting);
               else
                  This.Current_Setting := Settings'First;
               end if;
            elsif Event.Value < 0 then
               if This.Current_Setting /= Settings'First then
                  This.Current_Setting := Settings'Pred (This.Current_Setting);
               else
                  This.Current_Setting := Settings'Last;
               end if;
            end if;
            pragma Warnings (On, "condition can only be True");
      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding
   procedure On_Pushed (This  : in out Pattern_Settings_Menu)
   is
   begin
      null;
   end On_Pushed;

   --------------
   -- On_Focus --
   --------------

   overriding
   procedure On_Focus (This       : in out Pattern_Settings_Menu;
                       Exit_Value : Window_Exit_Value)
   is
   begin
      null;
   end On_Focus;

end WNM.GUI.Menu.Pattern_Settings;
