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

package WNM.GUI is

   procedure Update;

--     type Window is interface;
--     type Window_Ref is not null access Window'Class;
--
--     procedure Draw (This   : Window;
--                     Buffer : HAL.Bitmap.Bitmap_Buffer'Class)
--     is abstract;
--
--
--     type Pop_Up_Window is interface;
--     type Pop_Up_Window_Ref is not null access Pop_Up_Window'Class;
--
--     function Finished (This : Pop_Up_Window) return Boolean
--     is abstract;
--
--     procedure Set_Base_Window (Win : Window_Ref);
--     procedure Add_Pop_Up (Win : Pop_Up_Window_Ref);
--
end WNM.GUI;
