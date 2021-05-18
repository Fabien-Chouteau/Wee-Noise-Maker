package body WNM.MIDI.Queues is

   --------------------
   -- Sequencer_Push --
   --------------------

   procedure Sequencer_Push (Msg : Message) is
   begin
      Push (Synth_Queue, Msg);
      Push (MIDI_Out_Queue, Msg);
   end Sequencer_Push;

   ---------------
   -- Synth_Pop --
   ---------------

   procedure Synth_Pop is
   begin
      while not Empty (Synth_Queue) loop
         Process (Pop (Synth_Queue));
      end loop;
   end Synth_Pop;

   ------------------
   -- MIDI_Out_Pop --
   ------------------

   procedure MIDI_Out_Pop is
   begin
      while not Empty (MIDI_Out_Queue) loop
         Process (Pop (MIDI_Out_Queue));
      end loop;
   end MIDI_Out_Pop;

   ------------
   -- Inc_In --
   ------------

   procedure Inc_In (Q : in out Message_Queue) is
   begin
      if Q.Next_In = Q.Arr'Last then
         Q.Next_In := Q.Arr'First;
      else
         Q.Next_In := Q.Next_In + 1;
      end if;
   end Inc_In;

   -------------
   -- Inc_Out --
   -------------

   procedure Inc_Out (Q : in out Message_Queue) is
   begin
      if Q.Next_Out = Q.Arr'Last then
         Q.Next_Out := Q.Arr'First;
      else
         Q.Next_Out := Q.Next_Out + 1;
      end if;
   end Inc_Out;

   -----------
   -- Empty --
   -----------

   function Empty (Q : Message_Queue) return Boolean is
   begin
      return Q.Next_In = Q.Next_Out;
   end Empty;

   ----------
   -- Push --
   ----------

   procedure Push (Q : in out Message_Queue; Msg : Message) is
   begin
      Q.Arr (Q.Next_In) := Msg;

      Inc_In (Q);

      --  If after the push In = Out, it means the buffer was full and an old
      --  message was replaced. Move Out to skip.
      if Q.Next_In = Q.Next_Out then
         Inc_Out (Q);
      end if;
   end Push;

   ---------
   -- Pop --
   ---------

   function Pop (Q : in out Message_Queue) return Message is
      Ret : Message;
   begin
      Ret := Q.Arr (Q.Next_Out);
      Inc_Out (Q);
      return Ret;
   end Pop;

end WNM.MIDI.Queues;
