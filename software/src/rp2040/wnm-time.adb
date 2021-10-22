with RP.Device;
with RP.Timer;

with WNM.RP2040;
pragma Elaborate (WNM.RP2040);

package body WNM.Time is


   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      RP.Device.Timer.Enable;
   end Initialize;

   -----------
   -- Clock --
   -----------

   function Clock return Time_Ms
   is (Time_Ms (RP.Timer.Clock) / 1_000);

   --------------
   -- Delay_Ms --
   --------------

   procedure Delay_Ms (Milliseconds : UInt64) is
   begin
      RP.Device.Timer.Delay_Milliseconds (Integer (Milliseconds));
   end Delay_Ms;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (Wakeup_Time : Time_Ms) is
   begin
      RP.Device.Timer.Delay_Until (RP.Timer.Time (Wakeup_Time * 1_000));
   end Delay_Until;

begin
   Initialize;
end WNM.Time;
