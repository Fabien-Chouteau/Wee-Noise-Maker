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

with File_IO; use File_IO;

package body WNM.Sample_Library is

   procedure Load_Folder (Folder : Sample_Folders);

   function Valid_Entry (Index : Sample_Entry_Index) return Boolean
     is (Index <= Last_Entry);

   ------------------
   -- Folder_Range --
   ------------------

   function Folder_Range
     (Folder : Sample_Folders)
      return Sample_Folder_Range
   is (Folder_Ranges (Folder));

   ----------------------
   -- Folder_Full_Path --
   ----------------------

   function Folder_Full_Path (Folder : Sample_Folders) return String is
   begin
      return Root_Samples_Path & Folder_Path (Folder);
   end Folder_Full_Path;

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
         return Folder_Full_Path (Entries (Index).Folder) &
           Entry_Name (Index) & ".raw";
      else
         return "";
      end if;
   end Entry_Path;

   -----------------
   -- Load_Folder --
   -----------------

   procedure Load_Folder (Folder : Sample_Folders) is

      function Ext_Is_Raw (Filename : String) return Boolean
      is (Filename'Length > 4
          and then
          Filename (Filename'Last - 3 .. Filename'Last) = ".raw");

      Path   : constant String := Folder_Full_Path (Folder);
      DD     : Directory_Descriptor;
      Status : Status_Code;

   begin
      Status := Open (DD, Path);

      if Status /= OK then
         --  The folder is not here but we do not report any error
         return;
      end if;

      loop
         declare
            DE : constant Directory_Entry := Read (DD);
         begin

            exit when
                DE = Invalid_Dir_Entry
              or else
                Last_Entry = Sample_Entry_Index'Last;

            if Ext_Is_Raw (DE.Name)
              and then

                --  Is there enough room to store the name?
                Name_Buffer'Length - Name_Buffer_Cnt >= DE.Name'Length - 4
            then

               Last_Entry := Last_Entry + 1;

               Entries (Last_Entry).Folder := Folder;
               Entries (Last_Entry).Name_From := Name_Buffer_Cnt + 1;
               Entries (Last_Entry).Name_To   := Name_Buffer_Cnt + DE.Name'Length - 4;
               Name_Buffer_Cnt := Entries (Last_Entry).Name_To;

               --  Insert name in the Name_Buffer
               Name_Buffer (Entries (Last_Entry).Name_From .. Entries (Last_Entry).Name_To)
                 := DE.Name (DE.Name'First .. DE.Name'Last - 4);


               --  Update the folder range
               if Folder_Ranges (Folder).From = 0 then
                  Folder_Ranges (Folder).From := Last_Entry;
               end if;

               Folder_Ranges (Folder).To := Last_Entry;

            end if;
         end;
      end loop;

      Close (DD);
   end Load_Folder;

   ----------
   -- Load --
   ----------

   procedure Load is
   begin
      for Folder in Sample_Folders loop
         Load_Folder (Folder);
      end loop;
   end Load;

end WNM.Sample_Library;
