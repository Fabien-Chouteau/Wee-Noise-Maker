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

with WNM.GUI.Bitmap_Fonts;     use WNM.GUI.Bitmap_Fonts;
with WNM.Sample_Stream;
with WNM.Screen;
with Quick_Synth;

package body WNM.GUI.Menu.Sample_Trim is

   Sample_Trim_Singleton : aliased Trim_Window;

   -----------------
   -- Push_Window --
   -----------------

   procedure Push_Window is
   begin
      Push (Sample_Trim_Singleton'Access);
   end Push_Window;

   -----------
   -- Start --
   -----------

   function Start return Natural
   is (Sample_Trim_Singleton.Start);

   ----------
   -- Stop --
   ----------

   function Stop return Natural
   is (Sample_Trim_Singleton.Stop);

   --------------------
   -- Preview_Sample --
   --------------------

   procedure Preview_Sample (This : Trim_Window) is
   begin
      WNM.Sample_Stream.Assign_Sample (WNM.Sample_Stream.Always_On,
                                       Sample_Rec_Filepath);

      WNM.Sample_Stream.Start (Track       => WNM.Sample_Stream.Always_On,
                               Start_Point => This.Start,
                               End_Point   => This.Stop,
                               Looping     => False);
   end Preview_Sample;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (This : in out Trim_Window)
   is
      X : Integer := 5;
      Start : constant Natural := This.Start / This.Increment;
      Stop  : constant Natural := This.Stop / This.Increment;
   begin
--        Print (Buffer      => Screen.all,
--               X_Offset    => X,
--               Y_Offset    => 0,
--               Str         => "Trim sample");

      Screen.Draw_Line (Start     => (Start, 12),
                        Stop      => (Stop, 12));
      Screen.Draw_Line (Start     => (Start, 9),
                        Stop      => (Start, 15));
      Screen.Draw_Line (Start     => (Stop, 9),
                        Stop      => (Stop, 15));
      X := 5;
      Print (X_Offset    => X,
             Y_Offset    => 0,
             Str         => This.Start'Img);
      X := 5;
      Print (X_Offset    => X,
             Y_Offset    => 8,
             Str         => This.Stop'Img);
   end Draw;

   --------------
   -- On_Event --
   --------------

   overriding procedure On_Event
     (This  : in out Trim_Window;
      Event : Menu_Event)
   is
      Tmp : Integer;
   begin
      case Event.Kind is
         when Left_Press =>
            Pop (Exit_Value => Success);
         when Right_Press =>
            This.Preview_Sample;
         when Encoder_Right =>
            Tmp := This.Stop + Event.Value * This.Increment;

            This.Stop := (if Tmp < This.Start then
                             This.Start
                          elsif Tmp > This.Size then
                             This.Size
                          else
                             Tmp);

            This.Preview_Sample;

         when Encoder_Left =>
            Tmp := This.Start + Event.Value * This.Increment;

            This.Start := (if Tmp < 0 then
                             0
                          elsif Tmp > This.Stop then
                             This.Stop
                          else
                             Tmp);

            This.Start := This.Start - (This.Start mod WNM.Mono_Buffer_Size_In_Bytes);

            This.Preview_Sample;
      end case;
   end On_Event;

   ---------------
   -- On_Pushed --
   ---------------

   overriding procedure On_Pushed (This : in out Trim_Window)
   is
   begin

      This.Start := 0;
      This.Stop  := Quick_Synth.Record_Size;
      This.Size  := This.Stop;

      This.Increment := (This.Size / 95) + 1;

      if This.Increment = 0 then
         Pop (Exit_Value => Failure);
      end if;
   end On_Pushed;

end WNM.GUI.Menu.Sample_Trim;
