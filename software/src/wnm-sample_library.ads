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

   --  TODO: this package is not really needed. Its only purpose is to list the
   --  files in /samples/ for the sample select window. It could be replaced by
   --  a directory walk in the sample select window package.

   subtype Sample_Entry_Index is Natural range 0 .. 256;
   Invalid_Sample_Entry : constant Sample_Entry_Index :=
     Sample_Entry_Index'First;

   Root_Samples_Path : constant String := "/samples/";

   function Entry_Name (Index : Sample_Entry_Index) return String;
   function Entry_Path (Index : Sample_Entry_Index) return String;

   function User_Sample_Exists (Name : String) return Boolean;
   function Add_Sample (Name : String) return Sample_Entry_Index;

   function First_Valid_Entry return Sample_Entry_Index;
   function Last_Valid_Entry return Sample_Entry_Index;

   procedure Load;

private

   subtype Name_Index is Natural range 1 .. 1024;
   Name_Buffer     : String (Name_Index) := (others => ASCII.NUL);
   Name_Buffer_Cnt : Natural := 0;

   type Sample_Entry is record
      Name_From, Name_To : Natural := 0;
   end record with Pack;

   subtype Valid_Sample_Entry_Index is Sample_Entry_Index range
     Invalid_Sample_Entry + 1 .. Sample_Entry_Index'Last;

   Entries : array (Valid_Sample_Entry_Index) of Sample_Entry;
   Last_Entry : Natural := Invalid_Sample_Entry;

end WNM.Sample_Library;
