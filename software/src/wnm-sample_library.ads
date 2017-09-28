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

package WNM.Sample_Library is

   subtype Sample_Entry_Index is Natural range 0 .. 256;
   Invalid_Sample_Entry : constant Sample_Entry_Index := Sample_Entry_Index'First;

   type Sample_Folders is (Drums_Kick,
                           Drums_Snare,
                           Drums_Tom,
                           Drums_Cymbal,
                           Drums_Hat,
                           Drums_Clap,
                           Drums_Misc,
                           Vocals,
                           Misc,
                           User);

   Root_Samples_Path : constant String := "/sdcard/samples/";

   function Folder_Path (Folder : Sample_Folders) return String
   is (case Folder is
          when Drums_Kick   => "drums/kick/",
          when Drums_Snare  => "drums/snare/",
          when Drums_Tom    => "drums/tom/",
          when Drums_Cymbal => "drums/cymbal/",
          when Drums_Hat    => "drums/hat/",
          when Drums_Clap   => "drums/clap/",
          when Drums_Misc   => "drums/misc/",
          when Vocals       => "vocals/",
          when Misc         => "misc/",
          when User         => "user/");

   type Sample_Folder_Range is record
      From, To : Natural;
   end record;

   function Folder_Range (Folder : Sample_Folders) return Sample_Folder_Range;
   function Folder_Full_Path (Folder : Sample_Folders) return String;

   function Entry_Name (Index : Sample_Entry_Index) return String;
   function Entry_Path (Index : Sample_Entry_Index) return String;

   function Entry_From_Path (Path : String) return Sample_Entry_Index;

   function User_Sample_Exists (Name : String) return Boolean;
   function Add_User_Sample (Path : String) return Sample_Entry_Index;

   procedure Load;

private

   subtype Name_Index is Natural range 1 .. 1024;
   Name_Buffer     : String (Name_Index) := (others => ASCII.NUL);
   Name_Buffer_Cnt : Natural := 0;

   type Sample_Entry is record
      Name_From, Name_To : Natural := 0;
      Folder : Sample_Folders;
   end record with Pack;

   subtype Valid_Sample_Entry_Index is Sample_Entry_Index range
     Invalid_Sample_Entry + 1 .. Sample_Entry_Index'Last;

   Entries : array (Valid_Sample_Entry_Index) of Sample_Entry;
   Last_Entry : Natural := Invalid_Sample_Entry;

   Folder_Ranges : array (Sample_Folders) of Sample_Folder_Range :=
     (others => (Invalid_Sample_Entry, Invalid_Sample_Entry));

end WNM.Sample_Library;
