
package body Fuzzy is

   type Triangle is new Membership_Function with record
      Lower, Max, Higher : Scalar;
      Max_Value          : Membership;
   end record;
   overriding function Value (Self : Triangle; X : Scalar) return Membership;

   type Slope_Left is new Membership_Function with record
      Lower, Max : Scalar;
      Max_Value  : Membership;
   end record;
   overriding function Value (Self : Slope_Left; X : Scalar) return Membership;

   type Slope_Right is new Membership_Function with record
      Higher, Max : Scalar;
      Max_Value   : Membership;
   end record;
   overriding function Value
      (Self : Slope_Right; X : Scalar) return Membership;

   ---------------------
   -- Create_Triangle --
   ---------------------

   function Create_Triangle
      (Lower, Max, Higher : Scalar;
       Max_Value          : Membership := 1.0)
      return not null access Membership_Function'Class is
   begin
      return new Triangle'
         (Membership_Function with
          Lower => Lower, Max => Max, Higher => Higher,
          Max_Value => Max_Value);
   end Create_Triangle;

   -----------
   -- Value --
   -----------

   overriding function Value
      (Self : Triangle; X : Scalar) return Membership is
   begin
      if X <= Self.Lower or else X >= Self.Higher then
         return 0.0;
      elsif X = Self.Max then
         return Self.Max_Value;
      elsif X <= Self.Max then
         return Self.Max_Value
            * Membership ((X - Self.Lower) / (Self.Max - Self.Lower));
      else
         return Self.Max_Value
            * Membership ((X - Self.Higher) / (Self.Max - Self.Higher));
      end if;
   end Value;

   -----------------------
   -- Create_Slope_Left --
   -----------------------

   function Create_Slope_Left
      (Max, Lower : Scalar;
       Max_Value  : Membership := 1.0)
      return not null access Membership_Function'Class is
   begin
      return new Slope_Left'
         (Membership_Function with
          Lower => Lower, Max => Max, Max_Value => Max_Value);
   end Create_Slope_Left;

   -----------
   -- Value --
   -----------

   overriding function Value
      (Self : Slope_Left; X : Scalar) return Membership is
   begin
      if X <= Self.Max then
         return Self.Max_Value;
      elsif X <= Self.Lower then
         return Self.Max_Value
           * Membership ((X - Self.Lower) / (Self.Max - Self.Lower));
      else
         return 0.0;
      end if;
   end Value;

   ------------------------
   -- Create_Slope_Right --
   ------------------------

   function Create_Slope_Right
      (Higher, Max  : Scalar;
       Max_Value   : Membership := 1.0)
      return not null access Membership_Function'Class is
   begin
      return new Slope_Right'
         (Membership_Function with
          Higher => Higher, Max => Max, Max_Value => Max_Value);
   end Create_Slope_Right;

   -----------
   -- Value --
   -----------

   overriding function Value
      (Self : Slope_Right; X : Scalar) return Membership is
   begin
      if X <= Self.Higher then
         return 0.0;
      elsif X <= Self.Max then
         return Self.Max_Value
           * Membership ((X - Self.Higher) / (Self.Max - Self.Higher));
      else
         return Self.Max_Value;
      end if;
   end Value;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (Self : in out Variable; Name : String) is
   begin
      Self.Name := To_Unbounded_String (Name);
   end Set_Name;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Self : Variable) return String is
   begin
      return To_String (Self.Name);
   end Get_Name;

   ---------------
   -- Set_Range --
   ---------------

   procedure Set_Range (Self : in out Variable; Min, Max : Scalar) is
   begin
      Self.Min := Min;
      Self.Max := Max;
   end Set_Range;

   --------------
   -- Add_Term --
   --------------

   procedure Add_Term
      (Self       : in out Variable;
       Name       : String;
       Membership : not null access Membership_Function'Class)
   is
   begin
      Self.Terms.Append
         ((Name       => To_Unbounded_String (Name),
           Membership => Membership.all'Unrestricted_Access));
   end Add_Term;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Self : in out Input_Variable; Value : Scalar) is
   begin
      Self.Value := Value;
   end Set_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Self : Output_Variable) return Scalar is
   begin
      return Self.Value;
   end Get_Value;

   ---------------
   -- Add_Input --
   ---------------

   procedure Add_Input
      (Self : in out Engine; Var : not null access Input_Variable'Class) is
   begin
      Self.Inputs.Append (Var);
   end Add_Input;

   ----------------
   -- Add_Output --
   ----------------

   procedure Add_Output
      (Self : in out Engine; Var : not null access Output_Variable'Class) is
   begin
      Self.Outputs.Append (Var);
   end Add_Output;

   -------------
   -- Process --
   -------------

   procedure Process (Self : Engine) is
   begin
      raise Program_Error;
   end Process;

end Fuzzy;
