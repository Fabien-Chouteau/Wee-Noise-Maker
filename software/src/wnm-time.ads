with HAL;      use HAL;
--  with HAL.Time;

package WNM.Time is

   subtype Time_Ms is UInt64;

   function Clock return Time_Ms;

   procedure Delay_Ms (Milliseconds : UInt64);

   procedure Delay_Until (Wakeup_Time : Time_Ms);

--     function Tick_Period return Time_Ms;
--
--     type Tick_Callback is access procedure;
--
--     function Tick_Subscriber (Callback : not null Tick_Callback) return Boolean;
--     --  Return True if callback is already a tick event subscriber
--
--     function Tick_Subscribe (Callback : not null Tick_Callback) return Boolean
--       with Pre  => not Tick_Subscriber (Callback),
--            Post => (if Tick_Subscribe'Result then Tick_Subscriber (Callback));
--     --  Subscribe a callback to the tick event. The function return True on
--     --  success, False if there's no more room for subscribers.
--
--     function Tick_Unsubscribe (Callback : not null Tick_Callback) return Boolean
--       with Pre  => Tick_Subscriber (Callback),
--            Post => (if Tick_Unsubscribe'Result
--                       then not Tick_Subscriber (Callback));
--     --  Unsubscribe a callback to the tick event. The function return True on
--     --  success, False if the callback was not a subscriber.
--
--     function HAL_Delay return not null HAL.Time.Any_Delays;
--
--  private
--
--     type PG_Delays is new HAL.Time.Delays with null record;
--
--     overriding
--     procedure Delay_Microseconds (This : in out PG_Delays;
--                                   Us   : Integer);
--
--     overriding
--     procedure Delay_Milliseconds (This : in out PG_Delays;
--                                   Ms   : Integer);
--
--     overriding
--     procedure Delay_Seconds      (This : in out PG_Delays;
                                 --  S    : Integer);
end WNM.Time;
