with System;
with Littlefs; use Littlefs;
with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;
with WNM.Time;

with HAL; use HAL;

package body WNM.Storage is

   Block_Size : constant := 4096;
   Page_Size : constant := 256;

   Config : aliased LFS_Config;

   function Read (C      : access constant LFS_Config;
                  Block  : LFS_Block;
                  Off    : LFS_Offset;
                  Buffer : System.Address;
                  Size   : LFS_Size)
                  return int
     with Convention => C;

   function Prog (C      : access constant LFS_Config;
                  Block  : LFS_Block;
                  Off    : LFS_Offset;
                  Buffer : System.Address;
                  Size   : LFS_Size)
                  return int
     with Convention => C;
   function Erase (C     : access constant LFS_Config;
                   Block : LFS_Block)
                   return int
     with Convention => C;
   function Sync (C : access constant LFS_Config) return int
     with Convention => C;

   ----------
   -- Read --
   ----------

   function Read (C      : access constant LFS_Config;
                  Block  : LFS_Block;
                  Off    : LFS_Offset;
                  Buffer : System.Address;
                  Size   : LFS_Size)
                  return int
   is
   begin
      return LFS_ERR_IO;
   end Read;

   ----------
   -- Prog --
   ----------

   function Prog (C      : access constant LFS_Config;
                  Block  : LFS_Block;
                  Off    : LFS_Offset;
                  Buffer : System.Address;
                  Size   : LFS_Size)
                  return int
   is
   begin
      return LFS_ERR_IO;
   end Prog;

   -----------
   -- Erase --
   -----------

   function Erase (C : access constant LFS_Config;
                   Block : LFS_Block)
                   return int
   is
   begin
      return LFS_ERR_IO;
   end Erase;

   ----------
   -- Sync --
   ----------

   function Sync (C : access constant LFS_Config) return int is
      pragma Unreferenced (C);
   begin
      return LFS_ERR_IO;
   end Sync;

   --------------------
   -- Get_LFS_Config --
   --------------------

   function Get_LFS_Config return not null access Littlefs.LFS_Config is
   begin
      return Config'Access;
   end Get_LFS_Config;

begin
   Config.Context := System.Null_Address;
   Config.Read := Read'Access;
   Config.Prog := Prog'Access;
   Config.Erase := Erase'Access;
   Config.Sync := Sync'Access;
   Config.Read_Size := Block_Size;
   Config.Prog_Size := Block_Size;
   Config.Block_Size := Block_Size;
   Config.Block_Count := Size / Block_Size;
   Config.Block_Cycles := 700;
   Config.Cache_Size := Config.Block_Size;
   Config.Lookahead_Size := Config.Block_Size;
   Config.Read_Buffer := System.Null_Address;
   Config.Prog_Buffer := System.Null_Address;
   Config.Lookahead_Buffer := System.Null_Address;
   Config.Name_Max := 0;
   Config.File_Max := 0;
   Config.Attr_Max := 0;
end WNM.Storage;
