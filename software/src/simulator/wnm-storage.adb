with Littlefs; use Littlefs;
with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;

with System.Storage_Elements; use System.Storage_Elements;

package body WNM.Storage is

   Config : constant not null access LFS_Config := new LFS_Config;
   Buf : Storage_Array (1 .. Storage_Offset (Size));

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
      Offset : constant LFS_Offset := Off + C.Block_Size * LFS_Size (Block);

      Dev_Buf : Storage_Array (1 .. Storage_Offset (Size))
        with Address => To_Address
          (To_Integer (C.Context) + Integer_Address (Offset));

      Read_Buf : Storage_Array (1 .. Storage_Offset (Size))
        with Address => Buffer;
   begin
      Read_Buf := Dev_Buf;
      return 0;
   end Read;

   function Prog (C      : access constant LFS_Config;
                  Block  : LFS_Block;
                  Off    : LFS_Offset;
                  Buffer : System.Address;
                  Size   : LFS_Size)
                  return int
   is
      Offset : constant LFS_Offset := Off + C.Block_Size * LFS_Size (Block);

      Dev_Buf : Storage_Array (1 .. Storage_Offset (Size))
        with Address => To_Address
          (To_Integer (C.Context) + Integer_Address (Offset));

      Read_Buf : Storage_Array (1 .. Storage_Offset (Size))
        with Address => Buffer;
   begin
      Dev_Buf := Read_Buf;
      return 0;
   end Prog;

   function Erase (C : access constant LFS_Config;
                   Block : LFS_Block)
                   return int
   is
      pragma Unreferenced (Block, C);
   begin
      return 0;
   end Erase;

   function Sync (C : access constant LFS_Config) return int is
      pragma Unreferenced (C);
   begin
      return 0;
   end Sync;

   --------------------
   -- Get_LFS_Config --
   --------------------

   function Get_LFS_Config return not null access Littlefs.LFS_Config is
   begin
      return Config;
   end Get_LFS_Config;

begin
   Config.Context := Buf (Buf'First)'Address;
   Config.Read := Read'Access;
   Config.Prog := Prog'Access;
   Config.Erase := Erase'Access;
   Config.Sync := Sync'Access;
   Config.Read_Size := 2048;
   Config.Prog_Size := 2048;
   Config.Block_Size := 2048;
   Config.Block_Count := Size / 2048;
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
