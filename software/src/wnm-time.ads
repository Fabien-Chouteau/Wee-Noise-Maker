with HAL;      use HAL;
--  with HAL.Time;

package WNM.Time is

   subtype Time_Ms is UInt64;

   function Clock return Time_Ms;

   procedure Delay_Ms (Milliseconds : UInt64);

   procedure Delay_Until (Wakeup_Time : Time_Ms);

end WNM.Time;
