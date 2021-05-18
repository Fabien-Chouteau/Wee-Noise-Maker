with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;
with Littlefs; use Littlefs;
with WNM.Storage;
with WNM.Factory_Reset;

package body WNM.File_System is

   FS : aliased Littlefs.LFS_T;

   -----------
   -- Close --
   -----------

   procedure Close (FD : aliased in out File_Descriptor) is
   begin
      if Littlefs.Close (FS, FD) /= 0 then
         raise Program_Error with "Close file error...";
      end if;
   end Close;

   -----------------
   -- Create_File --
   -----------------

   procedure Create_File (FD : aliased in out File_Descriptor; Name : String) is
   begin
      if Littlefs.Open (FS, FD, Name, LFS_O_WRONLY + LFS_O_CREAT) /= 0 then
         raise Program_Error with "Create ('" & Name & "') file error...";
      end if;
   end Create_File;

   ---------------
   -- Open_Read --
   ---------------

   procedure Open_Read (FD : aliased in out File_Descriptor; Name : String) is
   begin
      if Littlefs.Open (FS, FD, Name, LFS_O_RDONLY) /= 0 then
         raise Program_Error with "Open_Read ('" & Name & "') file error...";
      end if;
   end Open_Read;

   ----------
   -- Size --
   ----------

   function Size (FD : aliased in out File_Descriptor) return File_Signed_Size
   is (Littlefs.Size (FS, FD));

   ----------
   -- Read --
   ----------

   function Read (FD : aliased in out File_Descriptor;
                  A  : System.Address;
                  N  : File_Size)
                  return File_Signed_Size
   is
   begin
      return Littlefs.Read (FS, FD, A, N);
   end Read;

   -----------
   -- Write --
   -----------

   function Write (FD : aliased in out File_Descriptor;
                   A  : System.Address;
                   N  : File_Size)
                   return File_Signed_Size
   is
   begin
      return Littlefs.Write (FS, FD, A, N);
   end Write;

   ----------
   -- Seek --
   ----------

   procedure Seek (FD     : aliased in out File_Descriptor;
                   Off    : Offset;
                   Whence : Seek_Whence)
   is
   begin
      if Littlefs.Seek (FS, FD, Off, Whence'Enum_Rep) < 0 then
         raise Program_Error with "Seek error...";
      end if;
   end Seek;

   ---------------
   -- Available --
   ---------------

   function Available return File_Signed_Size is
   begin
      return WNM.Storage.Size  - Littlefs.Size (FS) * 2048;
   end Available;

   --------------------------
   -- For_Each_File_In_Dir --
   --------------------------

   procedure For_Each_File_In_Dir (Dirpath : String) is
      Dir : aliased LFS_Dir;
      Err : int;
      Info : aliased Entry_Info;
   begin
      Err := Open (FS, Dir, Dirpath);

      if Err = 0 then
         while Read (FS, Dir, Info) > 0 loop
            declare
               Name : constant String := Littlefs.Name (Info);
            begin
               if Name /= "." and then Name /= ".." then
                  Process (Name);
               end if;
            end;
         end loop;
         Err := Close (FS, Dir);
      end if;
   end For_Each_File_In_Dir;

   Err : int;

   Do_Reset : constant Boolean := True;
begin
   if Do_Reset then
      Err := Littlefs.Format (FS, WNM.Storage.Get_LFS_Config.all);
      if Err /= 0 then
         raise Program_Error with "Format error:" & Err'Img;
      end if;
   end if;

   Err := Littlefs.Mount (FS, WNM.Storage.Get_LFS_Config.all);
   if Err /= 0 then
      raise Program_Error with "Mount error:" & Err'Img;
   end if;

   if Do_Reset then
      WNM.Factory_Reset.Reset (FS);
   end if;
end WNM.File_System;
