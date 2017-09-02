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

with WNM.GUI.Bitmap_Fonts; use WNM.GUI.Bitmap_Fonts;
with HAL.Bitmap;           use HAL.Bitmap;

package body WNM.GUI.Menu.Text_Dialog is

   Text_Dialog : aliased Text_Dialog_Window;

   -----------------------------
   -- Push_Text_Dialog_Window --
   -----------------------------

   procedure Push_Text_Dialog_Window is
   begin
      Push (Text_Dialog'Access);
   end Push_Text_Dialog_Window;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (This   : in out Text_Dialog_Window;
      Screen : not null HAL.Bitmap.Any_Bitmap_Buffer)
   is
      X        : Integer := 1;
      Select_X : constant Integer := X + (This.Index - This.Text'First) * 6;
   begin
      case This.Mode is
         when Text_Mode =>
            Print (Buffer      => Screen.all,
                   X_Offset    => X,
                   Y_Offset    => 0,
                   Str         => "Enter text:");

            X := 1;
            Print (Buffer      => Screen.all,
                   X_Offset    => X,
                   Y_Offset    => 9,
                   Str         => This.Text (This.Text'First .. This.Text'First + This.Len - 1),
                   Invert_From => (Select_X - 1),
                   Invert_To   => (Select_X + 5));

         when Confirm_Mode =>
            X := 1;
            Print (Buffer      => Screen.all,
                   X_Offset    => X,
                   Y_Offset    => 0,
                   Str         => "Confirm? : " & (if This.Confirm then "Yes" else "No"));
            X := 1;
            Print (Buffer      => Screen.all,
                   X_Offset    => X,
                   Y_Offset    => 9,
                   Str         => This.Text (This.Text'First .. This.Text'First + This.Len - 1));
      end case;
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding procedure On_Event
     (This  : in out Text_Dialog_Window;
      Event : Menu_Event)
   is
   begin
      case This.Mode is
         when Confirm_Mode =>
            case Event.Kind is
            when Left_Press =>
               if This.Confirm then
                  Menu.Pop;
               else
                  This.Mode := Text_Mode;
               end if;
            when Right_Press =>
               This.Mode := Text_Mode;
            when Encoder_Left =>
               This.Confirm := not This.Confirm;
            when others =>
               null;
            end case;
         when Text_Mode =>
            case Event.Kind is
            when Left_Press =>
               This.Mode := Confirm_Mode;
               This.Confirm := False;
            when Right_Press =>
               --  Delete last character
               if This.Len > 1 then
                  if This.Index = This.Text'First + This.Len - 1 then
                     This.Index := This.Index - 1;
                  end if;
                  This.Len := This.Len - 1;
               end if;

            when Encoder_Right =>

               --  Move cursor and increase length if necessary

               if Event.Value > 0 then
                  if This.Index < This.Text'First + This.Len - 1 then
                     This.Index := This.Index + 1;
                  elsif This.Index /= This.Text'Last then
                     This.Index := This.Index + 1;
                     This.Len   := This.Len + 1;
                     This.Text (This.Index) := 'A';
                  end if;
               elsif Event.Value < 0 then
                  if This.Index > This.Text'First then
                     This.Index := This.Index - 1;
                  end if;
               end if;
            when Encoder_Left =>
               if Event.Value > 0 then
                  This.Text (This.Index) := Character'Succ (This.Text (This.Index));
               elsif Event.Value < 0 then
                  This.Text (This.Index) := Character'Pred (This.Text (This.Index));
               end if;
            end case;
      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding procedure On_Pushed
     (This  : in out Text_Dialog_Window)
   is
   begin
      This.Len   := 1;
      This.Index := This.Text'First;
      This.Text (This.Text'First) := 'A';
      This.Mode := Text_Mode;
   end On_Pushed;

   --------------
   -- On_Focus --
   --------------

   overriding procedure On_Focus
     (This  : in out Text_Dialog_Window)
   is
   begin
      null;
   end On_Focus;

end WNM.GUI.Menu.Text_Dialog;
