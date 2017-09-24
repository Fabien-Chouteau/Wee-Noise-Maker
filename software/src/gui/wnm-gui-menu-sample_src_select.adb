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

with WNM.Sample_Stream;      use WNM.Sample_Stream;
with WNM.GUI.Bitmap_Fonts;   use WNM.GUI.Bitmap_Fonts;
with Enum_Next;

package body WNM.GUI.Menu.Sample_Src_Select is

   package Rec_Src_Enum_Next is new Enum_Next (Rec_Src);
   use Rec_Src_Enum_Next;

   Src_Select_Window_Singleton : aliased Src_Select_Menu;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Src_Select_Window_Singleton'Access);
   end Push_Window;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (This   : in out Src_Select_Menu;
      Screen : not null HAL.Bitmap.Any_Bitmap_Buffer)
   is
      X : Integer := 5;
   begin
      Print (Buffer      => Screen.all,
             X_Offset    => X,
             Y_Offset    => 0,
             Str         => "Source:");

      X := 5;
      Print (Buffer      => Screen.all,
             X_Offset    => X,
             Y_Offset    => 9,
             Str         => This.Src'Img);

      --  Print_Percentage (Down, "Volume", This.Volume);
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding procedure On_Event
     (This  : in out Src_Select_Menu;
      Event : Menu_Event)
   is
      Tmp : Integer;
   begin
      case Event.Kind is
         when Left_Press =>
            Sample_Stream.Start_Recording
              (Filename => Sample_Rec_Filepath,
               Source   => This.Src,
               Max_Size => 332000 * 10 * 2);
            Menu.Pop (Exit_Value => Success);
         when Right_Press =>
            Menu.Pop (Exit_Value => Failure);
         when Encoder_Right =>
            Tmp := This.Volume + Event.Value;
            if Tmp < 0 then
               This.Volume := 0;
            elsif Tmp > 100 then
               This.Volume := 100;
            else
               This.Volume := Natural (Tmp);
            end if;
         when Encoder_Left =>
            if Event.Value > 0 then
               This.Src := Next (This.Src);
            elsif Event.Value < 0 then
               This.Src := Prev (This.Src);
            end if;
      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding procedure On_Pushed
     (This  : in out Src_Select_Menu)
   is
      pragma Unreferenced (This);
   begin
      null;
   end On_Pushed;

   --------------
   -- On_Focus --
   --------------

   overriding procedure On_Focus
     (This       : in out Src_Select_Menu;
      Exit_Value : Window_Exit_Value)
   is
      pragma Unreferenced (This);
   begin
      null;
   end On_Focus;

end WNM.GUI.Menu.Sample_Src_Select;
