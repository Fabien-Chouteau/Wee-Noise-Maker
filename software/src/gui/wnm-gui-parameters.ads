package WNM.GUI.Parameters is

   type Parameter_Slot is (Up, Down);

   procedure Print_Int (Slot  : Parameter_Slot;
                        Name  : String;
                        Value : Integer;
                        Min   : Integer;
                        Max   : Integer);

   subtype Percentage is Natural range 0 .. 100;

   procedure Print_Percentage (Slot  : Parameter_Slot;
                               Name  : String;
                               Value : Natural);

   subtype Pan is Integer range -100 .. 100;

   procedure Print_Pan (Slot  : Parameter_Slot;
                        Name  : String;
                        Value : Pan);

end WNM.GUI.Parameters;
