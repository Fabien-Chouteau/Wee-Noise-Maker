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
   function Starts_With (Str, Prefix : String) return Boolean;
   function Sub_String (Str : String; Remove_Len : Natural) return String;

   function Ext_Is_Raw (Filename : String) return Boolean
   is (Filename'Length > 4
       and then
       Filename (Filename'Last - 3 .. Filename'Last) = ".raw");

   function Valid_Entry (Index : Sample_Entry_Index) return Boolean
     is (Index /= Invalid_Sample_Entry and then Index <= Last_Entry);

   function Add_Sample (Folder : Sample_Folders; Name : String)
                        return Sample_Entry_Index;

   -----------------
   -- Starts_With --
   -----------------

   function Starts_With (Str, Prefix : String) return Boolean is
   begin
      return Str'Length > Prefix'Length
        and then
          Str (Str'First .. Str'First + Prefix'Length - 1) = Prefix;
   end Starts_With;

   ----------------
   -- Sub_String --
   ----------------

   function Sub_String (Str : String; Remove_Len : Natural) return String
   is (Str (Str'First + Remove_Len .. Str'Last));

   ----------------
   -- Add_Sample --
   ----------------

   function Add_Sample (Folder : Sample_Folders; Name : String)
                        return Sample_Entry_Index
   is
   begin
      --  Is there enough room to store the name?
      if Name_Buffer'Length - Name_Buffer_Cnt >= Name'Length then

         Last_Entry := Last_Entry + 1;

         Entries (Last_Entry).Folder    := Folder;
         Entries (Last_Entry).Name_From := Name_Buffer_Cnt + 1;
         Entries (Last_Entry).Name_To   := Name_Buffer_Cnt + Name'Length;
         Name_Buffer_Cnt := Entries (Last_Entry).Name_To;

         --  Insert name in the Name_Buffer
         Name_Buffer (Entries (Last_Entry).Name_From .. Entries (Last_Entry).Name_To)
           := Name;

         --  Update the folder range
         if Folder_Ranges (Folder).From = Invalid_Sample_Entry then
            Folder_Ranges (Folder).From := Last_Entry;

         elsif Folder_Ranges (Folder).To /= Last_Entry - 1 then
            raise Program_Error with "Cannot add an entry to this folder anymore...";
         end if;

         Folder_Ranges (Folder).To := Last_Entry;

         return Last_Entry;
      end if;
      return Invalid_Sample_Entry;
   end Add_Sample;

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

   ---------------------
   -- Entry_From_Path --
   ---------------------

   function Entry_From_Path (Path : String) return Sample_Entry_Index is
   begin
      if not Starts_With (Path, Root_Samples_Path) then
         return Invalid_Sample_Entry;
      end if;

      declare
         Sub : constant String := Sub_String (Path, Root_Samples_Path'Length);
      begin
         --  Try to find the folder
         for Folder in Sample_Folders loop
            if Starts_With (Sub, Folder_Path (Folder)) then
               declare
                  Sample : constant String :=
                    Sub_String (Sub, Folder_Path (Folder)'Length);
               begin
                  for Index in Folder_Range (Folder).From .. Folder_Range (Folder).To loop
                     if Entry_Name (Index) & ".raw" = Sample then
                        return Index;
                     end if;
                  end loop;
               end;
               return Invalid_Sample_Entry;
            end if;
         end loop;
      end;

      return Invalid_Sample_Entry;
   end Entry_From_Path;

   ------------------------
   -- User_Sample_Exists --
   ------------------------

   function User_Sample_Exists (Name : String) return Boolean is
   begin

      for Index in Folder_Range (User).From .. Folder_Range (User).To loop
         if Entry_Name (Index) = Name then
            return True;
         end if;
      end loop;

      return False;
   end User_Sample_Exists;

   ---------------------
   -- Add_User_Sample --
   ---------------------

   function Add_User_Sample (Path : String) return Sample_Entry_Index is
   begin
      if Starts_With (Path, Folder_Full_Path (User))
        and then
          Ext_Is_Raw (Path)
      then
         declare
            Name : constant String :=
              Path (Path'First + Folder_Full_Path (User)'Length .. Path'Last - 4);
         begin
            return Add_Sample (User, Name);
         end;
      end if;
      return Invalid_Sample_Entry;
   end Add_User_Sample;

   -----------------
   -- Load_Folder --
   -----------------

   procedure Load_Folder (Folder : Sample_Folders) is

      Path   : constant String := Folder_Full_Path (Folder);
      DD     : Directory_Descriptor;
      Status : Status_Code;
      Unref  : Sample_Entry_Index with Unreferenced;
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

            if Ext_Is_Raw (DE.Name) then
               Unref := Add_Sample (Folder, DE.Name (DE.Name'First .. DE.Name'Last - 4));
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
