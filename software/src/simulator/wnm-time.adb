with Ada.Calendar; use Ada.Calendar;

package body WNM.Time is

   Start_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;

   function Clock return Time_Ms is
      Delt_Sec : constant Duration := Ada.Calendar.Clock - Start_Time;
   begin
      return UInt64 (Delt_Sec * 1000.0);
   end Clock;

   --------------
   -- Delay_Ms --
   --------------

   procedure Delay_Ms (Milliseconds : UInt64) is
   begin
      delay Duration (Milliseconds) / 1_000;
   end Delay_Ms;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (Wakeup_Time : Time_Ms) is
      Now : constant Time_Ms := Clock;
   begin
      if Wakeup_Time > Now then
         Delay_Ms (Wakeup_Time - Now);
      end if;
   end Delay_Until;

end WNM.Time;
