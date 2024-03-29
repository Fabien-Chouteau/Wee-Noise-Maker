--------------------------------------------------------
-- This file is autogenerated by pioasm; do not edit! --
--------------------------------------------------------

pragma Style_Checks (Off);

with RP.PIO;

package WNM.RP2040.PIO_Rotary_Encoder_ASM is

   ------------------------
   -- Pio_Rotary_Encoder --
   ------------------------

   Pio_Rotary_Encoder_Wrap_Target : constant := 0;
   Pio_Rotary_Encoder_Wrap        : constant := 23;

   Pio_Rotary_Encoder_Program_Instructions : RP.PIO.Program := (
                    --  .wrap_target
         16#0011#,  --   0: jmp    17              side 0     
         16#0015#,  --   1: jmp    21              side 0     
         16#0017#,  --   2: jmp    23              side 0     
         16#0011#,  --   3: jmp    17              side 0     
         16#0011#,  --   4: jmp    17              side 0     
         16#0011#,  --   5: jmp    17              side 0     
         16#0011#,  --   6: jmp    17              side 0     
         16#0011#,  --   7: jmp    17              side 0     
         16#0011#,  --   8: jmp    17              side 0     
         16#0011#,  --   9: jmp    17              side 0     
         16#0011#,  --  10: jmp    17              side 0     
         16#0011#,  --  11: jmp    17              side 0     
         16#0011#,  --  12: jmp    17              side 0     
         16#0017#,  --  13: jmp    23              side 0     
         16#0015#,  --  14: jmp    21              side 0     
         16#0011#,  --  15: jmp    17              side 0     
         16#4002#,  --  16: in     pins, 2         side 0     
         16#a0e6#,  --  17: mov    osr, isr        side 0     
         16#60c2#,  --  18: out    isr, 2          side 0     
         16#4802#,  --  19: in     pins, 2         side 1     
         16#a086#,  --  20: mov    exec, isr       side 0     
         16#d010#,  --  21: irq    nowait 0 rel    side 2     
         16#0011#,  --  22: jmp    17              side 0     
         16#d012#); --  23: irq    nowait 2 rel    side 2     
                    --  .wrap

end WNM.RP2040.PIO_Rotary_Encoder_ASM;
