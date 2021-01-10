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

with WNM.File_System;

package body WNM.Sample_Library is

   function Valid_Entry (Index : Sample_Entry_Index) return Boolean
     is (Index /= Invalid_Sample_Entry and then Index <= Last_Entry);

   -----------------------
   -- First_Valid_Entry --
   -----------------------

   function First_Valid_Entry return Sample_Entry_Index
   is (if Last_Entry = Invalid_Sample_Entry
       then Invalid_Sample_Entry
       else Valid_Sample_Entry_Index'First);

   ----------------------
   -- Last_Valid_Entry --
   ----------------------

   function Last_Valid_Entry return Sample_Entry_Index
   is (Last_Entry);

   ----------------
   -- Add_Sample --
   ----------------

   function Add_Sample (Name : String) return Sample_Entry_Index
   is
   begin
      --  Is there enough room to store the name?
      if Name_Buffer'Length - Name_Buffer_Cnt >= Name'Length then

         Last_Entry := Last_Entry + 1;
         Entries (Last_Entry).Name_From := Name_Buffer_Cnt + 1;
         Entries (Last_Entry).Name_To   := Name_Buffer_Cnt + Name'Length;
         Name_Buffer_Cnt := Entries (Last_Entry).Name_To;

         --  Insert name in the Name_Buffer
         Name_Buffer (Entries (Last_Entry).Name_From .. Entries (Last_Entry).Name_To)
           := Name;

         return Last_Entry;
      end if;
      return Invalid_Sample_Entry;
   end Add_Sample;

   ----------------
   -- Entry_Name --
   ----------------

   function Entry_Name (Index : Sample_Entry_Index) return String is
   begin
      if Valid_Entry (Index) then
         return Name_Buffer (Entries (Index).Name_From .. Entries (Index).Name_To);
      else
         return "";
      end if;
   end Entry_Name;

   ----------------
   -- Entry_Path --
   ----------------

   function Entry_Path (Index : Sample_Entry_Index) return String is
   begin
      if Valid_Entry (Index) then
         return Root_Samples_Path & Entry_Name (Index);
      else
         return "";
      end if;
   end Entry_Path;

   ------------------------
   -- User_Sample_Exists --
   ------------------------

   function User_Sample_Exists (Name : String) return Boolean is
   begin

      for Index in Valid_Sample_Entry_Index'First .. Last_Entry loop
         if Entry_Name (Index) = Name then
            return True;
         end if;
      end loop;

      return False;
   end User_Sample_Exists;

   ----------
   -- Load --
   ----------

   procedure Load is
      procedure Add_To_Library (Filename : String) is
         Unused : Sample_Entry_Index;
      begin
         Unused := Add_Sample (Filename);
      end Add_To_Library;

      procedure For_Each_File
      is new WNM.File_System.For_Each_File_In_Dir (Add_To_Library);

   begin
      For_Each_File (Root_Samples_Path);
   end Load;

end WNM.Sample_Library;
