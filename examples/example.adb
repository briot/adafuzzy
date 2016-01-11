with Fuzzy;
with GNATCOLL.Traces;   use GNATCOLL.Traces;

procedure Example is
   Me : constant Trace_Handle := Create ("FUZZY");

   package Fuzzys is new Fuzzy (Scalar => Float, Base_Membership => Float);
   use Fuzzys;

   Temperature : access Input_Variable;
   Power       : access Output_Variable;
   Eng         : Engine;

begin
   GNATCOLL.Traces.Parse_Config_File;

   Temperature := new Input_Variable;
   Temperature.Set_Name ("Temperature");
   Temperature.Set_Range (-30.0, 50.0);
   Temperature.Add_Term ("Cold",   Create_Ramp_Left (10.0, 15.0));
   Temperature.Add_Term ("Medium", Create_Triangle (12.0, 20.0, 28.0));
   Temperature.Add_Term ("Hot",    Create_Ramp_Right (25.0, 30.0));
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

   Temperature.Set_Value (14.0);

   for Iterate in 1 .. 100_000 loop
      Eng.Process;

      --  Put_Line ("-----------------------------------------");
      --  Put_Line ("Temperature=" & Temperature.Get_Value'Img);
      --  Put_Line ("Result => Power is set to" & Power.Get_Value'Img);

      --  Simulate the effect of the power
      --  100% power => +0.5 degree
      --  0% power   => -0.5 degree
      Temperature.Set_Value
         (Temperature.Get_Value +
          (Power.Get_Value - 50.0) * 0.01);
   end loop;

   Free (Eng);
   GNATCOLL.Traces.Finalize;
end Example;
