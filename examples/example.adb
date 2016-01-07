with Fuzzy;
with Ada.Text_IO;   use Ada.Text_IO;

procedure Example is
   package Fuzzys is new Fuzzy (Scalar => Float, Base_Membership => Float);
   use Fuzzys;

   Temperature : access Input_Variable;
   Power       : access Output_Variable;
   Eng         : Engine;

begin
   Temperature := new Input_Variable;
   Temperature.Set_Name ("Temperature");
   Temperature.Set_Range (-30.0, 50.0);
   Temperature.Add_Term ("Cold",   Create_Slope_Left (10.0, 15.0));
   Temperature.Add_Term ("Medium", Create_Triangle (12.0, 20.0, 25.0));
   Temperature.Add_Term ("Hot",    Create_Slope_Right (22.0, 30.0));
   Eng.Add_Input (Temperature);

   Power := new Output_Variable;
   Power.Set_Name ("Power");
   Power.Set_Range (0.0, 100.0);
   Power.Add_Term ("Low",    Create_Slope_Left (20.0, 50.0));
   Power.Add_Term ("Medium", Create_Triangle (30.0, 50.0, 70.0));
   Power.Add_Term ("High",   Create_Slope_Right (60.0, 80.0));
   Eng.Add_Output (Power);

   Eng.Add_Rule_Block
      (Name   => "Rules 1",
       Rules  => (Implies (Temperature = "Cold", Power = "High"),
                  Implies (Temperature = "Medium", Power = "Medium"),
                  Implies (Temperature = "Hot", Power = "Low")));

   Temperature.Set_Value (14.0);
   Eng.Process;
   Put_Line (Power.Get_Value'Img);

end Example;
