with Ada.Containers;   use Ada.Containers;
with Ada.Unchecked_Deallocation;

package body Fuzzy is
   Me : constant Trace_Handle := Create ("FUZZY");

   Debug : constant Boolean := True;
   --  Whether to enable debug traces.
   --  If you set this to True, you also need to enable the traces by calling
   --  GNATCOLL.Traces.Parse_Config_File and create an appropriate config
   --  file. Disabling the traces provides a performance gain even when
   --  nothing is printed.

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

   type Ramp_Left is new Membership_Function with record
      Lower, Max : Scalar;
      Max_Value  : Membership;
   end record;
   overriding function Value (Self : Ramp_Left; X : Scalar) return Membership;

   type Ramp_Right is new Membership_Function with record
      Higher, Max : Scalar;
      Max_Value   : Membership;
   end record;
   overriding function Value
      (Self : Ramp_Right; X : Scalar) return Membership;

   type Activated_Membership is new Membership_Function with record
      Wrapped    : not null access Membership_Function'Class;
      Activation : Activation_Method;
      Value      : Membership;
   end record;
   overriding function Value
      (Self : Activated_Membership; X : Scalar) return Membership;
   --  An internal wrapper for membership functions, which applies the
   --  activation functions and defuzzification methods automatically. This
   --  perform full computation of centroid, ... by doing some limited form
   --  of numeric integration.

   type Accumulated_Membership (Max_Rules_Count : Rule_Idx)
   is new Membership_Function with record
      Funcs        : Membership_Function_Array (1 .. Max_Rules_Count);
      Last         : Natural := 0;
      Accumulation : Accumulation_Method;
   end record;
   overriding function Value
      (Self : Accumulated_Membership; X : Scalar) return Membership;
   overriding procedure Free (Self : in out Accumulated_Membership);
   overriding function Defuzzify
      (Self   : not null access Accumulated_Membership;
       Method : Defuzzification_Method;
       Min, Max : Scalar) return Scalar;
   --  An internal membership that represents the accumulation of several
   --  membership functions that were activated by the rules.

   function Is_Empty (Self : Accumulated_Membership'Class) return Boolean
      is (Self.Last < Natural (Self.Funcs'First)) with Inline;
   --  True if there are no accumulated function

   procedure Add_Func
      (Self : in out Accumulated_Membership'Class;
       Func : not null access Membership_Function'Class);
   --  Add a new function to accumulate

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      (Membership_Function'Class, Membership_Function_Access);

   function Hedge_Very (X : Membership) return Membership;
   function Hedge_Not (X : Membership) return Membership;
   --  Implementation for the hedge functions

   procedure Freeze (Self : in out Engine'Class);
   --  Make it invalid to add new variables or terms to this engine

   procedure Free (R : in out Rule_Block);
   --  Free the memory used by R (but not by the next rule block)

   procedure Free (Self : in out Variable_Access);
   procedure Free (Self : in out Rule);
   --  Free the memory used by Self

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
   -- Create_Ramp_Left --
   -----------------------

   function Create_Ramp_Left
      (Max, Lower : Scalar;
       Max_Value  : Membership := 1.0)
      return not null access Membership_Function'Class is
   begin
      return new Ramp_Left'
         (Membership_Function with
          Lower => Lower, Max => Max, Max_Value => Max_Value);
   end Create_Ramp_Left;

   -----------
   -- Value --
   -----------

   overriding function Value
      (Self : Ramp_Left; X : Scalar) return Membership is
   begin
      if X <= Self.Max then
         return Self.Max_Value;
      elsif X < Self.Lower then
         return Self.Max_Value
           * Membership ((X - Self.Lower) / (Self.Max - Self.Lower));
      else
         return 0.0;
      end if;
   end Value;

   ------------------------
   -- Create_Ramp_Right --
   ------------------------

   function Create_Ramp_Right
      (Higher, Max  : Scalar;
       Max_Value   : Membership := 1.0)
      return not null access Membership_Function'Class is
   begin
      return new Ramp_Right'
         (Membership_Function with
          Higher => Higher, Max => Max, Max_Value => Max_Value);
   end Create_Ramp_Right;

   -----------
   -- Value --
   -----------

   overriding function Value
      (Self : Ramp_Right; X : Scalar) return Membership is
   begin
      if X <= Self.Higher then
         return 0.0;
      elsif X < Self.Max then
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
      (Self : Activated_Membership; X : Scalar) return Membership is
   begin
      case Self.Activation is
         when Activation_Min =>
            return Membership'Min (Self.Value, Self.Wrapped.Value (X));
         when Activation_Prod =>
            return Self.Value * Self.Wrapped.Value (X);
      end case;
   end Value;

   -----------
   -- Value --
   -----------

   overriding function Value
      (Self : Accumulated_Membership; X : Scalar) return Membership
   is
      Result : Membership;
   begin
      if Self.Is_Empty then
         return 0.0;
      else
         Result := Self.Funcs (Self.Funcs'First).Value (X);
         case Self.Accumulation is
            when Accumulation_Max =>
               for F in Self.Funcs'First + 1 .. Rule_Idx (Self.Last) loop
                  Result := Membership'Max (Result, Self.Funcs (F).Value (X));
               end loop;

            when Accumulation_Sum =>
               for F in Self.Funcs'First + 1 .. Rule_Idx (Self.Last) loop
                  Result := Membership'Min
                     (1.0, Result + Self.Funcs (F).Value (X));
               end loop;
         end case;
      end if;
      return Result;
   end Value;

   ---------------
   -- Defuzzify --
   ---------------

   overriding function Defuzzify
      (Self   : not null access Accumulated_Membership;
       Method : Defuzzification_Method;
       Min, Max : Scalar) return Scalar is
   begin
      if Self.Is_Empty then
         return 0.0;

      elsif Self.Last = Natural (Self.Funcs'First) then
         --  If there is a single rule, defer to that rule's primitive
         --  which might be more efficient
         return Self.Funcs (Self.Funcs'First).Defuzzify (Method, Min, Max);

      else
         --  Use the default implementation that does heavier computation
         return Membership_Function (Self.all).Defuzzify (Method, Min, Max);
      end if;
   end Defuzzify;

   --------------
   -- Add_Func --
   --------------

   procedure Add_Func
      (Self : in out Accumulated_Membership'Class;
       Func : not null access Membership_Function'Class) is
   begin
      Self.Last := Self.Last + 1;
      Self.Funcs (Rule_Idx (Self.Last)) := Membership_Function_Access (Func);
   end Add_Func;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Accumulated_Membership) is
   begin
      for A in Self.Funcs'First .. Rule_Idx (Self.Last) loop
         Unchecked_Free (Self.Funcs (A));
      end loop;
      Self.Last := Natural (Self.Funcs'First) - 1;
   end Free;

   --------------
   -- Activate --
   --------------

   function Activate
      (Self   : not null access Membership_Function;
       Method : Activation_Method;
       Value  : Membership)
      return not null access Membership_Function'Class is
   begin
      return new Activated_Membership'
         (Membership_Function with
          Wrapped    => Self,
          Activation => Method,
          Value      => Value);
   end Activate;

   ---------------
   -- Defuzzify --
   ---------------

   function Defuzzify
      (Self   : not null access Membership_Function;
       Method : Defuzzification_Method;
       Min, Max : Scalar) return Scalar
   is
      Dx, X, Y, Xcog, Weight : Scalar;
   begin
      case Method is
         when Defuzzify_Centroid =>
            Dx     := (Max - Min) / Scalar (Resolution);
            Xcog   := 0.0;
            Weight := 0.0;
            X      := Min + 0.5 * Dx;

            for J in 1 .. Resolution loop
               --  value in the middle of the interval
               Y := Scalar (Membership_Function_Access (Self).Value (X));
               Xcog := Xcog + X * Y;
               Weight := Weight + Y;  --  multiply by Dx only once at the end
               X := X + Dx;
            end loop;

            return Xcog / Weight;
      end case;
   end Defuzzify;

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

   procedure Set_Value
      (Self   : Variable'Class;
       Values : in out Variable_Values;
       Value  : Scalar)
   is
   begin
      Values.Values (Self.Idx) := Value;
   end Set_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
      (Self : Variable'Class; Values : Variable_Values) return Scalar is
   begin
      return Values.Values (Self.Idx);
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
      return (Var, To_Unbounded_String (Term));
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
      VIdx : Var_Idx := Var_Idx'First;
      Idx  : Fuzzy_Var_Idx := Fuzzy_Var_Idx'First;
   begin
      for Var of Self.Inputs loop
         Var.Frozen := True;
         Var.Idx := VIdx;
         VIdx := VIdx + 1;

         for T of Var.Terms loop
            T.Idx := Idx;
            Idx := Idx + 1;
         end loop;
      end loop;

      Self.Total_Input_And_Terms := Idx - 1;

      for Var of Self.Outputs loop
         Var.Frozen := True;
         Var.Idx := VIdx;
         VIdx := VIdx + 1;
      end loop;
   end Freeze;

   -----------
   -- Trace --
   -----------

   procedure Trace (Me : Trace_Handle; Self : Engine) is
      Debug : Unbounded_String;
      R     : Rule_Block;
   begin
      if Active (Me) then
         Increase_Indent (Me, "Input variables:");
         for V of Self.Inputs loop
            Debug := V.Name;
            for T of V.Terms loop
               Append (Debug, " " & T.Name & " (in"
                  & T.In_Rules'Img & " rules, idx="
                  & T.Idx'Img & ")");
            end loop;
            Trace (Me, To_String (Debug));
         end loop;
         Decrease_Indent (Me);

         Increase_Indent (Me, "Output variables");
         for V of Self.Outputs loop
            Debug := V.Name;
            for T of V.Terms loop
               Append (Debug, " " & T.Name & " ("
                  & T.In_Rules'Img & " rules)");
            end loop;
            Trace (Me, To_String (Debug));
         end loop;
         Decrease_Indent (Me);

         R := Self.Rules;
         while R /= null loop
            Increase_Indent (Me, "Rule block " & To_String (R.Name));
            for U in R.Left'Range (1) loop
               Debug := To_Unbounded_String
                  ("rule" & U'Img & " antecedents idx=");
               for V in R.Left'Range (2) loop
                  Append (Debug, R.Left (U, V).Fuzzy_Value'Img);
               end loop;
               Trace (Me, To_String (Debug));
            end loop;
            Decrease_Indent (Me);
            R := R.Next;
         end loop;
      end if;
   end Trace;

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
      Member : Membership_Function_Access;

   begin
      if not Self.Has_Rules then
         Freeze (Self);
         Self.Total_Rules := Rules'Length;
      else
         Self.Total_Rules := Self.Total_Rules + Rules'Length;
      end if;

      Tmp := new Rule_Block_Details
         (Rules_Count       => Rules'Length,
          Input_Vars_Count  => Var_Idx (Self.Inputs.Length),
          Output_Vars_Count => Var_Idx (Self.Outputs.Length));
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

            if Idx = Var_Not_Found then
               raise Invalid_Variable with
                  "Variable """ & To_String (Input.Var.Name)
                  & """ not in this engine";
            elsif Value = Term_Not_Found then
               raise Invalid_Term with
                  "Term """ & To_String (Input.Term.Term)
                  & """ invalid for variable """
                  & To_String (Input.Var.Name) & """";
            elsif Tmp.Left (R_Idx, Idx).Fuzzy_Value /= Term_Not_Found then
               raise Invalid_Variable with
                  "Variable """ & To_String (Input.Var.Name)
                  & """ is referenced twice in a rule";
            else
               Tmp.Left (R_Idx, Idx) :=
                  (Fuzzy_Value => Value,
                   Hedge       => Input.Term.Func);
            end if;
         end loop;

         --  Process output variables

         for Output of Rules (R).Output.all loop
            Idx    := Var_Not_Found;
            Member := null;

            For_All_Outputs :
            for C in Self.Outputs.Iterate loop
               if Element (C) = Output.Var then
                  Idx := To_Index (C);
                  for T of Self.Outputs.Reference (C).Terms loop
                     if T.Name = Output.Term then
                        Member := T.Membership;
                        T.In_Rules := T.In_Rules + 1;
                        exit For_All_Outputs;
                     end if;
                  end loop;
                  exit For_All_Outputs;
               end if;
            end loop For_All_Outputs;

            if Idx = Var_Not_Found then
               raise Invalid_Variable with
                  "Variable """ & To_String (Output.Var.Name)
                  & """ not in this engine";
            elsif Member = null then
               raise Invalid_Term with
                  "Term """ & To_String (Output.Term)
                  & """ invalid for variable """
                  & To_String (Output.Var.Name) & """";
            elsif Tmp.Right (R_Idx, Idx).Term /= null then
               raise Invalid_Variable with
                  "Variable """ & To_String (Output.Var.Name)
                  & """ is referenced twice in a rule";
            else
               Tmp.Right (R_Idx, Idx) :=
                  (Term        => Member);
            end if;
         end loop;
      end loop;

      --  Free the rules, which are no longer needed

      declare
         Tmp_Rules : Rule_Array := Rules;
      begin
         for R in Tmp_Rules'Range loop
            Free (Tmp_Rules (R));
         end loop;
      end;
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

   -------------------------
   -- Set_Defuzzification --
   -------------------------

   procedure Set_Defuzzification
      (Self   : in out Engine;
       Method : Defuzzification_Method := Defuzzify_Centroid) is
   begin
      Self.Defuzzification := Method;
   end Set_Defuzzification;

   -------------
   -- Process --
   -------------

   procedure Process (Self : Engine; Values : in out Variable_Values) is
      Rule_Levels : Membership_Array (1 .. Self.Total_Rules);
      --  Activation level for each rules

      Vars : Fuzzy_Var_Values
         (Fuzzy_Var_Idx'First .. Self.Total_Input_And_Terms);

      procedure Fuzzify;
      --  Compute the fuzzifyed value for the input variables
      --  Sets:  Vars

      procedure Activate;
      --  Compute the total activation level for each rule by combining
      --  all their antecedents, hedges and weights.
      --  Sets: Rule_Levels

      procedure Accumulate_And_Defuzzify;
      --  For each output variable, look at the all rules that set it,
      --  and compute the final membership function. Defuzzify the
      --  value to a crisp value

      -------------
      -- Fuzzify --
      -------------

      procedure Fuzzify is
      begin
         for V of Self.Inputs loop
            for T of V.Terms loop
               if T.In_Rules /= 0 then
                  Vars (T.Idx) := T.Membership.Value (V.Get_Value (Values));
               end if;
            end loop;
         end loop;
      end Fuzzify;

      --------------
      -- Activate --
      --------------

      procedure Activate is
         B : Rule_Block;
         M, Tmp : Membership;
         Idx : Fuzzy_Var_Idx;
         R_Idx  : Rule_Idx := Rule_Idx'First;
      begin
         B := Self.Rules;
         while B /= null loop
            if B.Enabled then
               for R in B.Left'Range (1) loop
                  M := Membership'Last;
                  for Input in B.Left'Range (2) loop
                     Idx := B.Left (R, Input).Fuzzy_Value;

                     if Idx /= Term_Not_Found then
                        Tmp := Vars (Idx);
                        if B.Left (R, Input).Hedge /= null then
                           Tmp := B.Left (R, Input).Hedge (Tmp);
                        end if;

                        M := B.And_Operator (M, Tmp);
                     end if;
                  end loop;

                  Rule_Levels (R_Idx) := M * B.Weights (R);
                  R_Idx := R_Idx + 1;
               end loop;
            end if;
            B := B.Next;
         end loop;
      end Activate;

      ------------------------------
      -- Accumulate_And_Defuzzify --
      ------------------------------

      procedure Accumulate_And_Defuzzify is
         R_Idx : Rule_Idx;
         B : Rule_Block;
         Member : Membership_Function_Access;
         Accumulated : aliased Accumulated_Membership (Self.Total_Rules);
         Var : Output_Variable_Access;
      begin
         Accumulated.Accumulation := Self.Accumulation;

         --  for each output variable, look at all the rules

         for Output in 1 .. Var_Idx (Self.Outputs.Length) loop
            R_Idx := Rule_Idx'First;
            B := Self.Rules;

            --  Activate all rules: compute the membership function for
            --  the current output variable, for each rule.

            while B /= null loop
               if B.Enabled then
                  for R in B.Right'Range (1) loop
                     Member := B.Right (R, Output).Term;
                     if Member /= null then
                        Accumulated.Add_Func
                           (Member.Activate
                              (B.Activation, Value => Rule_Levels (R_Idx)));
                     end if;

                     R_Idx := R_Idx + 1;
                  end loop;
               end if;
               B := B.Next;
            end loop;

            Var := Self.Outputs (Output);
            Var.Set_Value
               (Values,
                Accumulated.Defuzzify
                   (Self.Defuzzification, Var.Min, Var.Max));

            Accumulated.Free;
         end loop;
      end Accumulate_And_Defuzzify;

   begin
      Fuzzify;
      Activate;
      Accumulate_And_Defuzzify;

      if Debug and then Active (Me) then
         declare
            Debug : Unbounded_String;
         begin
            Increase_Indent (Me, "Process");
            for V of Self.Inputs loop
               Debug := V.Name & " value="
                  & V.Get_Value (Values)'Img & " fuzzy=";
               for T of V.Terms loop
                  Append (Debug, Vars (T.Idx)'Img & "/" & T.Name);
               end loop;
               Trace (Me, To_String (Debug));
            end loop;

            Debug := To_Unbounded_String ("Rule activation:");
            for R of Rule_Levels loop
               Append (Debug, R'Img);
            end loop;
            Trace (Me, To_String (Debug));

            for V of Self.Outputs loop
               Trace (Me, To_String (V.Name)
                      & " value=" & V.Get_Value (Values)'Img);
            end loop;

            Decrease_Indent (Me);
         end;
      end if;
   end Process;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Engine) is
      B, B_Next : Rule_Block;
   begin
      for V of Self.Inputs loop
         Free (Variable_Access (V));
      end loop;
      Self.Inputs.Clear;

      for V of Self.Outputs loop
         Free (Variable_Access (V));
      end loop;
      Self.Outputs.Clear;

      B := Self.Rules;
      Self.Rules := null;
      while B /= null loop
         B_Next := B.Next;
         Free (B);
         B := B_Next;
      end loop;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (R : in out Rule_Block) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (Rule_Block_Details, Rule_Block);
   begin
      Unchecked_Free (R);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Variable_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (Membership_Function'Class, Membership_Function_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (Variable'Class, Variable_Access);
   begin
      if Self /= null then
         for T of Self.Terms loop
            Free (T.Membership.all);
            Unchecked_Free (T.Membership);
         end loop;
         Unchecked_Free (Self);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Rule) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (Input_Expr_Array, Input_Expr_Array_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (Output_Expr_Array, Output_Expr_Array_Access);
   begin
      if Self.Input /= null then
         Unchecked_Free (Self.Input);
      end if;

      if Self.Output /= null then
         Unchecked_Free (Self.Output);
      end if;
   end Free;

   ----------------
   -- Get_Values --
   ----------------

   function Get_Values (Self : Engine'Class) return Variable_Values is
   begin
      return Variable_Values'
         (Var_Count => Var_Idx (Self.Number_Of_Variables),
          Values    => (others => 0.0));
   end Get_Values;

   -------------------------
   -- Number_Of_Variables --
   -------------------------

   function Number_Of_Variables (Self : Engine'Class) return Natural is
   begin
      return Natural (Self.Inputs.Length + Self.Outputs.Length);
   end Number_Of_Variables;

end Fuzzy;
