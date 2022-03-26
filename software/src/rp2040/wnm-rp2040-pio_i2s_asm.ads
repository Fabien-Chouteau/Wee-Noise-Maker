--------------------------------------------------------
-- This file is autogenerated by pioasm; do not edit! --
--------------------------------------------------------

pragma Style_Checks (Off);

with RP.PIO;

package WNM.RP2040.PIO_I2S_ASM is

   ---------------
   -- Audio_I2s --
   ---------------

   Audio_I2s_Wrap_Target : constant := 0;
   Audio_I2s_Wrap        : constant := 15;

   Offset_entry_point : constant := 15;

   Audio_I2s_Program_Instructions : RP.PIO.Program := (
                    --  .wrap_target
         16#7001#,  --   0: out    pins, 1         side 2     
         16#b042#,  --   1: nop                    side 2     
         16#5801#,  --   2: in     pins, 1         side 3     
         16#1840#,  --   3: jmp    x--, 0          side 3     
         16#6001#,  --   4: out    pins, 1         side 0     
         16#a042#,  --   5: nop                    side 0     
         16#4801#,  --   6: in     pins, 1         side 1     
         16#e82e#,  --   7: set    x, 14           side 1     
         16#6001#,  --   8: out    pins, 1         side 0     
         16#a042#,  --   9: nop                    side 0     
         16#4801#,  --  10: in     pins, 1         side 1     
         16#0848#,  --  11: jmp    x--, 8          side 1     
         16#7001#,  --  12: out    pins, 1         side 2     
         16#b042#,  --  13: nop                    side 2     
         16#5801#,  --  14: in     pins, 1         side 3     
         16#f82e#); --  15: set    x, 14           side 3     
                    --  .wrap

end WNM.RP2040.PIO_I2S_ASM;
