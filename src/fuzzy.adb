with Ada.Containers;   use Ada.Containers;
with Ada.Text_IO;      use Ada.Text_IO;

package body Fuzzy is

   type Triangle is new Membership_Function with record
      Lower, Max, Higher : Scalar;
      Max_Value          : Membership;
   end record;
   overriding function Value (Self : Triangle; X : Scalar) return Membership;

   type Trapeze is new Membership_Function with record
      Lower, Max1, Max2, Higher : Scalar;
      Max_Value : Membership;
   end record;
   overriding function Value (Self : Trapeze; X : Scalar) return Membership;

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

   type Wrapper is new Membership_Function with record
      Wrapped    : not null access Membership_Function'Class;
      Activation : Activation_Method;
      Value      : Membership;
   end record;
   overriding function Value
      (Self : Wrapper; X : Scalar) return Membership;
   --  An internal wrapper for membership functions, which applies the
   --  activation functions and defuzzification methods automatically. This
   --  perform full computation of centroid, ... by doing some limited form
   --  of numeric integration.

   function Hedge_Very (X : Membership) return Membership;
   function Hedge_Not (X : Membership) return Membership;
   --  Implementation for the hedge functions

   procedure Freeze (Self : in out Engine'Class);
   --  Make it invalid to add new variables or terms to this engine

   procedure Fuzzify (Self : in out Engine'Class);
   --  Compute the fuzzifyed value for the input variables

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

   --------------------
   -- Create_Trapeze --
   --------------------

   function Create_Trapeze
      (Lower, Max1, Max2, Higher : Scalar;
       Max_Value                 : Membership := 1.0)
      return not null access Membership_Function'Class is
   begin
      return new Trapeze'
         (Membership_Function with
          Lower => Lower, Max1 => Max1, Max2 => Max2, Higher => Higher,
          Max_Value => Max_Value);
   end Create_Trapeze;

   -----------
   -- Value --
   -----------

   overriding function Value (Self : Trapeze; X : Scalar) return Membership is
   begin
      if X <= Self.Lower or else X >= Self.Higher then
         return 0.0;
      elsif X <= Self.Max1 then
         return Self.Max_Value
           * Membership ((X - Self.Lower) / (Self.Max1 - Self.Lower));
      elsif X <= Self.Max2 then
         return Self.Max_Value;
      else
         return Self.Max_Value
           * Membership ((X - Self.Higher) / (Self.Max2 - Self.Higher));
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

   -----------
   -- Value --
   -----------

   overriding function Value
      (Self : Wrapper; X : Scalar) return Membership is
   begin
      case Self.Activation is
         when Activation_Min =>
            return Membership'Min (Self.Value, Self.Wrapped.Value (X));
         when Activation_Prod =>
            return Self.Value * Self.Wrapped.Value (X);
      end case;
   end Value;

   --------------
   -- Activate --
   --------------

   function Activate
      (Self   : not null access Membership_Function;
       Method : Activation_Method;
       Value  : Membership)
      return Membership_Function'Class is
   begin
      return Wrapper'
         (Membership_Function with
          Wrapped    => Self,
          Activation => Method,
          Value      => Value);
   end Activate;

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
           Membership => Membership.all'Unrestricted_Access,
           In_Rules   => 0,
           Idx        => Term_Not_Found));
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

   ---------------
   -- Is_Frozen --
   ---------------

   function Is_Frozen (Self : Variable'Class) return Boolean is
   begin
      return Self.Frozen;
   end Is_Frozen;

   ---------------
   -- Has_Rules --
   ---------------

   function Has_Rules (Self : Engine'Class) return Boolean is
   begin
      return Self.Rules /= null;
   end Has_Rules;

   ---------------
   -- Hedge_Not --
   ---------------

   function Hedge_Not (X : Membership) return Membership is
   begin
      return Membership'Last - X;
   end Hedge_Not;

   ----------------
   -- Hedge_Very --
   ----------------

   function Hedge_Very (X : Membership) return Membership is
   begin
      return X * X;
   end Hedge_Very;

   -----------
   -- "not" --
   -----------

   function "not" (Term : String) return Term_With_Hedge is
   begin
      return (To_Unbounded_String (Term), Hedge_Not'Access);
   end "not";

   ----------
   -- Very --
   ----------

   function Very (Term : String) return Term_With_Hedge is
   begin
      return (To_Unbounded_String (Term), Hedge_Very'Access);
   end Very;

   ---------
   -- "=" --
   ---------

   function "="
      (Var    : not null access Input_Variable'Class;
       Term   : String) return Input_Expr is
   begin
      return (Var, (To_Unbounded_String (Term), null));
   end "=";

   ---------
   -- "=" --
   ---------

   function "="
      (Var    : not null access Input_Variable'Class;
       Term   : Term_With_Hedge) return Input_Expr is
   begin
      return (Var, Term);
   end "=";

   ---------
   -- "=" --
   ---------

   function "="
      (Var    : not null access Output_Variable'Class;
       Term   : String) return Output_Expr is
   begin
      return (Var, (To_Unbounded_String (Term), null));
   end "=";

   ---------
   -- "=" --
   ---------

   function "="
      (Var    : not null access Output_Variable'Class;
       Term   : Term_With_Hedge) return Output_Expr is
   begin
      return (Var, Term);
   end "=";

   -------------
   -- Implies --
   -------------

   function Implies
      (Input  : Input_Expr_Array;
       Output : Output_Expr_Array;
       Weight : Membership := 1.0) return Rule is
   begin
      return (Input  => new Input_Expr_Array'(Input),
              Output => new Output_Expr_Array'(Output),
              Weight => Weight);
   end Implies;

   -------------
   -- Implies --
   -------------

   function Implies
      (Input  : Input_Expr;
       Output : Output_Expr_Array;
       Weight : Membership := 1.0) return Rule is
   begin
      return (Input  => new Input_Expr_Array'((1 => Input)),
              Output => new Output_Expr_Array'(Output),
              Weight => Weight);
   end Implies;

   -------------
   -- Implies --
   -------------

   function Implies
      (Input  : Input_Expr;
       Output : Output_Expr;
       Weight : Membership := 1.0) return Rule is
   begin
      return (Input  => new Input_Expr_Array'((1 => Input)),
              Output => new Output_Expr_Array'((1 => Output)),
              Weight => Weight);
   end Implies;

   -------------
   -- Implies --
   -------------

   function Implies
      (Input  : Input_Expr_Array;
       Output : Output_Expr;
       Weight : Membership := 1.0) return Rule is
   begin
      return (Input  => new Input_Expr_Array'(Input),
              Output => new Output_Expr_Array'((1 => Output)),
              Weight => Weight);
   end Implies;

   ------------
   -- Freeze --
   ------------

   procedure Freeze (Self : in out Engine'Class) is
      Idx : Fuzzy_Var_Idx := Fuzzy_Var_Idx'First;
   begin
      for Var of Self.Inputs loop
         Var.Frozen := True;
         for T of Var.Terms loop
            T.Idx := Idx;
            Idx := Idx + 1;
         end loop;
      end loop;

      for Var of Self.Outputs loop
         Var.Frozen := True;
         for T of Var.Terms loop
            T.Idx := Idx;
            Idx := Idx + 1;
         end loop;
      end loop;

      Self.Vars := new Fuzzy_Var_Values (Fuzzy_Var_Idx'First .. Idx - 1);
   end Freeze;

   --------------------
   -- Add_Rule_Block --
   --------------------

   procedure Add_Rule_Block
      (Self  : in out Engine;
       Name  : String;
       Rules : Rule_Array;
       And_Operator : Operator := And_As_Min'Access;
       Activation   : Activation_Method := Activation_Min)
   is
      use Input_Variable_Vectors;
      use Output_Variable_Vectors;
      use Term_Vectors;
      Var_Not_Found  : constant Var_Idx := Var_Idx'Last;
      Tmp   : Rule_Block;
      Idx   : Var_Idx;
      Value : Fuzzy_Var_Idx;
      R_Idx : Rule_Idx;

      procedure Add_Variable
         (Var   : not null access Variable'Class;
          Term  : Term_With_Hedge);
      --  Add the variable we found (Idx, Found_Value) to the current rule

      procedure Add_Variable
         (Var   : not null access Variable'Class;
          Term  : Term_With_Hedge) is
      begin
         if Idx = Var_Not_Found then
            raise Invalid_Variable with
               "Variable """ & To_String (Var.Name)
               & """ not in this engine";
         elsif Value = Term_Not_Found then
            raise Invalid_Term with
               "Term """ & To_String (Term.Term)
               & """ invalid for variable """ & To_String (Var.Name) & """";
         elsif Tmp.Rules (R_Idx, Idx).Fuzzy_Value /= Term_Not_Found then
            raise Invalid_Variable with
               "Variable """ & To_String (Var.Name)
               & """ is referenced twice in a rule";
         else
            Tmp.Rules (R_Idx, Idx) :=
               (Fuzzy_Value => Value,
                Hedge       => Term.Func);
         end if;
      end Add_Variable;

   begin
      if not Self.Has_Rules then
         Freeze (Self);
      end if;

      Tmp := new Rule_Block_Details
         (Rules_Count => Rules'Length,
          Vars_Count  => Var_Idx (Self.Inputs.Length + Self.Outputs.Length));
      Tmp.Next := Self.Rules;
      Self.Rules := Tmp;

      Tmp.Name         := To_Unbounded_String (Name);
      Tmp.And_Operator := And_Operator;
      Tmp.Activation   := Activation;

      --  Process all rules

      for R in Rules'Range loop
         R_Idx := Rule_Idx (R - Rules'First + Integer (Tmp.Weights'First));
         Tmp.Weights (R_Idx) := Rules (R).Weight;

         --  Process input variables

         for Input of Rules (R).Input.all loop
            Idx   := Var_Not_Found;
            Value := Term_Not_Found;

            For_All_Inputs :
            for C in Self.Inputs.Iterate loop
               if Element (C) = Input.Var then
                  Idx := To_Index (C);
                  for T of Self.Inputs.Reference (C).Terms loop
                     if T.Name = Input.Term.Term then
                        Value := T.Idx;
                        T.In_Rules := T.In_Rules + 1;
                        exit For_All_Inputs;
                     end if;
                  end loop;
                  exit For_All_Inputs;
               end if;
            end loop For_All_Inputs;

            Add_Variable (Input.Var, Input.Term);
         end loop;

         --  Process output variables

         for Output of Rules (R).Output.all loop
            Idx   := Var_Not_Found;
            Value := Term_Not_Found;

            For_All_Outputs :
            for C in Self.Outputs.Iterate loop
               if Element (C) = Output.Var then
                  Idx := To_Index (C) + Var_Idx (Self.Inputs.Length);
                  for T of Self.Outputs.Reference (C).Terms loop
                     if T.Name = Output.Term.Term then
                        Value := T.Idx;
                        T.In_Rules := T.In_Rules + 1;
                        exit For_All_Outputs;
                     end if;
                  end loop;
                  exit For_All_Outputs;
               end if;
            end loop For_All_Outputs;

            Add_Variable (Output.Var, Output.Term);
         end loop;
      end loop;

      Put_Line ("Inputs:");
      for V of Self.Inputs loop
         Put_Line ("  " & To_String (V.Name));
         for T of V.Terms loop
            Put_Line ("    " & To_String (T.Name) & " (rules: "
               & T.In_Rules'Img & ") (idx=" & T.Idx'Img & ")");
         end loop;
      end loop;

      Put_Line ("Outputs:");
      for V of Self.Outputs loop
         Put_Line ("  " & To_String (V.Name));
         for T of V.Terms loop
            Put_Line ("    " & To_String (T.Name) & " (rules: "
               & T.In_Rules'Img & ") (idx=" & T.Idx'Img & ")");
         end loop;
      end loop;

      declare
         R : Rule_Block := Self.Rules;
      begin
         while R /= null loop
            Put_Line ("Rules: " & To_String (R.Name));
            for U in R.Rules'Range (1) loop
               Put ("  indexes=");
               for V in R.Rules'Range (2) loop
                  Put (R.Rules (U, V).Fuzzy_Value'Img);
               end loop;
               New_Line;
            end loop;
            R := R.Next;
         end loop;
      end;

      --  ??? Must free the rules
   end Add_Rule_Block;

   ----------------------
   -- Set_Accumulation --
   ----------------------

   procedure Set_Accumulation
      (Self : in out Engine; Method : Accumulation_Method := Accumulation_Max)
   is
   begin
      Self.Accumulation := Method;
   end Set_Accumulation;

   -------------
   -- Fuzzify --
   -------------

   procedure Fuzzify (Self : in out Engine'Class) is
   begin
      for V of Self.Inputs loop
         for T of V.Terms loop
            if T.In_Rules /= 0 then
               Self.Vars (T.Idx) := T.Membership.Value (V.Value);
            end if;
         end loop;
      end loop;

      Put_Line ("Fuzzify:");
      Put (" ");
      for V in Self.Vars'Range loop
         Put (Self.Vars (V)'Img);
      end loop;
      New_Line;
   end Fuzzify;

   -------------
   -- Process --
   -------------

   procedure Process (Self : in out Engine) is
      B : Rule_Block;
      M, Tmp : Membership;
      Idx : Fuzzy_Var_Idx;
      Input_Count : constant Var_Idx := Var_Idx (Self.Inputs.Length);
      Output_Count : constant Var_Idx := Var_Idx (Self.Outputs.Length);
   begin
      Put_Line ("Input variables:");
      for V of Self.Inputs loop
         Put_Line ("  " & To_String (V.Name) & " value=" & V.Value'Img);
      end loop;

      Fuzzify (Self);

      B := Self.Rules;
      while B /= null loop
         if B.Enabled then

            for R in B.Rules'Range (1) loop
               M := Membership'Last;
               for Input in 1 .. Input_Count loop
                  Idx := B.Rules (R, Input).Fuzzy_Value;

                  if Idx /= Term_Not_Found then
                     Tmp := Self.Vars (Idx);
                     if B.Rules (R, Input).Hedge /= null then
                        Tmp := B.Rules (R, Input).Hedge (Tmp);
                     end if;

                     M := B.And_Operator (M, Tmp);
                  end if;
               end loop;

               M := M * B.Weights (R);

               for Output in 1 .. Output_Count loop
                  Idx := B.Rules (R, Output + Input_Count).Fuzzy_Value;
                  if Idx /= Term_Not_Found then
                     declare
                        Out_Set : Membership_Function'Class :=
                           Self.Outputs (Output).Activate
                              (B.Activation, Value => M);
                     begin
                        null;
                     end;

                  end if;
               end loop;
            end loop;
         end if;

         B := B.Next;
      end loop;

      Put_Line ("Rule outputs:");
      Put (" ");
      for V in Self.Vars'Range loop
         Put (Self.Vars (V)'Img);
      end loop;
   end Process;

end Fuzzy;
