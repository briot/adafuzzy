with Ada.Numerics.Float_Random;  use Ada.Numerics.Float_Random;
with Fuzzy;
with GNATCOLL.Traces;   use GNATCOLL.Traces;
with Ada.Text_IO;       use Ada.Text_IO;

procedure Example is
   Me : constant Trace_Handle := Create ("FUZZY");

   package Fuzzys is new Fuzzy (Scalar => Float, Base_Membership => Float);
   use Fuzzys;

   Temperature : access Input_Variable;
   Power       : access Output_Variable;
   Eng         : Engine;

   Expected    : constant Float := 20.0;   --  Expected output temperature

   Float_Gen   : Generator;
   Till_Next_Nudge : Integer;
begin
   GNATCOLL.Traces.Parse_Config_File;

   Temperature := new Input_Variable;
   Temperature.Set_Name ("Temperature");
   Temperature.Set_Range (-30.0, 50.0);
   Temperature.Add_Term ("Cold",   Create_Ramp_Left (10.0, Expected));
   Temperature.Add_Term
      ("Medium", Create_Triangle (Expected - 1.0, Expected, Expected + 1.0));
   Temperature.Add_Term ("Hot",    Create_Ramp_Right (Expected, 30.0));
   Eng.Add_Input (Temperature);

   Power := new Output_Variable;
   Power.Set_Name ("Power");
   Power.Set_Range (0.0, 100.0);
   Power.Add_Term ("Low",    Create_Ramp_Left (20.0, 50.0));
   Power.Add_Term ("Medium", Create_Triangle (30.0, 50.0, 70.0));
   Power.Add_Term ("High",   Create_Ramp_Right (60.0, 80.0));
   Eng.Add_Output (Power);

   Eng.Add_Rule_Block
      (Name   => "Rules 1",
       Rules  => (Implies (Temperature = "Cold", Power = "High"),
                  Implies (Temperature = "Medium", Power = "Medium"),
                  Implies (Temperature = "Hot", Power = "Low")));

   Trace (Me, Eng);

   declare
      Values : Variable_Values := Eng.Get_Values;
   begin
      Temperature.Set_Value (Values, 14.0);

      Till_Next_Nudge := 30;
      Reset (Float_Gen);

      for Iterate in 1 .. 100_000 loop
         Eng.Process (Values);
         Put_Line ("Temp=" & Temperature.Get_Value (Values)'Img
            & "  Power=>" & Power.Get_Value (Values)'Img);

         --  Put_Line ("-----------------------------------------");
         --  Put_Line ("Temperature=" & Temperature.Get_Value'Img);
         --  Put_Line ("Result => Power is set to" & Power.Get_Value'Img);

         --  Simulate the effect of the power
         --  100% power => +0.5 degree   (radiators full on)
         --  0% power   => -0.5 degree   (cold from outside sipping in)
         Temperature.Set_Value
            (Values,
             Temperature.Get_Value (Values) +
             (Power.Get_Value (Values) - 50.0) * 0.01);

         --  From time to time, nudge at random to make things more interesting
         Till_Next_Nudge := Till_Next_Nudge - 1;
         if Till_Next_Nudge = 0 then
            Put_Line ("Nudge temperature");
            Till_Next_Nudge := 30;
            Temperature.Set_Value
               (Values,
                Temperature.Get_Value (Values)
                + (Random (Float_Gen) - 0.5) * 3.0);
         end if;
      end loop;
   end;

   Free (Eng);
   GNATCOLL.Traces.Finalize;
end Example;
