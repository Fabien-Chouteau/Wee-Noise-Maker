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

with MIDI;
with WNM;
with WNM.Sample_Library;
with Interfaces;         use Interfaces;

package Quick_Synth is

   type Mono_Sample is new Integer_16 with Size => 16;

   type Stereo_Sample is record
      L, R : Mono_Sample;
   end record with Pack, Size => 32;

   type Mono_Buffer is array (1 .. WNM.Samples_Per_Buffer) of Mono_Sample
     with Pack, Size => WNM.Mono_Buffer_Size_In_Bytes * 8;

   type Stereo_Buffer is array (1 .. WNM.Samples_Per_Buffer) of Stereo_Sample
     with Pack, Size => WNM.Stereo_Buffer_Size_In_Bytes * 8;

   procedure Trig (Track : WNM.Tracks);

   procedure Event (Msg : MIDI.Message);

   procedure Mute (Track : WNM.Tracks);
   procedure Unmute (Track : WNM.Tracks);
   procedure Toggle_Mute (Track : WNM.Tracks);
   function Muted (Track : WNM.Tracks) return Boolean;

   procedure Toggle_Solo (Track : WNM.Tracks);
   function In_Solo return Boolean;
   function Solo return WNM.Tracks;

   procedure Change_Pan (Track : WNM.Tracks;
                         Pan   : Integer);
   function Pan (Track : WNM.Tracks) return Integer;

   procedure Change_Volume (Track  : WNM.Tracks;
                            Volume : Integer);
   function Volume (Track : WNM.Tracks) return Natural;

   procedure Load_Samples;
   procedure Assign_Sample (Track       : WNM.Tracks;
                            Sample_Path : String);
   function Sample_Of_Track (Track : WNM.Tracks)
                             return WNM.Sample_Library.Sample_Entry_Index;

   procedure Update;

   ---------------
   -- Recording --
   ---------------

   type Rec_Source is (None, Input, Master_Output);

   function Now_Recording return Rec_Source;

   procedure Start_Recording (Filename : String;
                              Source   : Rec_Source;
                              Max_Size : Positive)
     with Pre => Now_Recording = None and then Source /= None;

   procedure Stop_Recording
     with Post => Now_Recording = None;

   function Record_Size return Natural;
   --  with Pre => Now_Recording /= None;

end Quick_Synth;
