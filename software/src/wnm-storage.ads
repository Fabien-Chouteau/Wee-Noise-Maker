with Littlefs;

package WNM.Storage is

   Size : constant := 1 * 1024 * 1024;

   function Get_LFS_Config return not null access Littlefs.LFS_Config;

end WNM.Storage;
