
--  This package provides a fuzzy inference engine.

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

generic
   type Scalar is digits <>;
   --  The base type that represents the crisps values.
   --  It will in general be a float, although it would be possible to use
   --  a fixed pointer or integer depending on your needs and performance
   --  requirements.

   type Base_Membership is digits <>;
   --  The base type to describe memberships. This type must include the
   --  range 0.0 .. 1.0, which is used to find the degree of membership in
   --  fuzzy sets. Performance can be tweaked by changing the precision of
   --  this type.

   Resolution : Integer := 200;
   --  This is used when performing integration on membership functions.
   --  Higher values will lead to more precise results, but slower
   --  computation.

package Fuzzy is

   type Membership is new Base_Membership range 0.0 .. 1.0;

   type Activation_Method is (Activation_Min, Activation_Prod);
   --  How the membership value of a rule's antecedents impact the rule's
   --  output sets.
   --    Activation_Min: The output sets are truncated so that their maximum
   --       membership value is that of the rule's antecedents.
   --    Activation_Scale: the output sets are scaled down.

   type Accumulation_Method is (Accumulation_Max, Accumulation_Sum);
   --  How to combine the multiple rules that apply to a variable
   --    Accumulation_Max: for each value in the output space, the maximum
   --       membership value for all the rules that fired is chosen.
   --    Accumulation_Sum: the bounded sum of all membership values is
   --       chosen.

   type Defuzzification_Method is (Defuzzify_Centroid);
   --  How to compute a crisp value from a fuzzy set
   --    Defuzzify_Centroid: find the center of gravity for the set.

   --------------------------
   -- Membership functions --
   --------------------------

   type Membership_Function is abstract tagged private;

   function Value
      (Self : Membership_Function; X : Scalar) return Membership is abstract;
   --  Returns the degree of membership of X to the set.

   function Activate
      (Self   : not null access Membership_Function;
       Method : Activation_Method;
       Value  : Membership)
      return not null access Membership_Function'Class;
   --  Return a new membership function that is the result of applying the
   --  activation method to Self.
   --  In some cases, it is possible to return an efficient function (for
   --  instance a Trapeze is created when we use Activation_Min on a Triangle.
   --  In other cases, the default implementation will do the computation on
   --  the fly (integration, centroid, ...) as needed, which is less efficient

   function Defuzzify
      (Self   : not null access Membership_Function;
       Method : Defuzzification_Method;
       Min, Max : Scalar) return Scalar;
   --  Compute a single crisp value to represent the set.
   --  The default is to do the computation on the fly (integration, ...), but
   --  more efficient ways exist for specific sets.

   -------------------------------------
   -- Predefined membership functions --
   -------------------------------------

   function Create_Triangle
      (Lower, Max, Higher : Scalar;
       Max_Value          : Membership := 1.0)
      return not null access Membership_Function'Class;
   --  Create a new triangle membership function.
   --         max_value
   --           /\
   --          /  \
   --    _____/    \________
   --        L  M  High

   function Create_Trapeze
      (Lower, Max1, Max2, Higher : Scalar;
       Max_Value                 : Membership := 1.0)
      return not null access Membership_Function'Class;
   --  Create a new trapeze membership function
   --           _______   max_value
   --          /       \
   --         /         \
   --   _____/           \_________
   --       L  M1     M2 H
   --
   --  Lower might be Scalar'First or Higher might be Scalar'Last if
   --  you want to emulate the standard logic  "x > High" set. However, it is
   --  more efficient to use a slop in this case.

   function Create_Slope_Left
      (Max, Lower : Scalar;
       Max_Value  : Membership := 1.0)
      return not null access Membership_Function'Class;
   --  Create a slope membership function, open to the left
   --
   --   ________    max_value
   --           \
   --            \
   --             \_________________
   --           M  L

   function Create_Slope_Right
      (Higher, Max : Scalar;
       Max_Value   : Membership := 1.0)
      return not null access Membership_Function'Class;
   --  Create a slop membership function, open to the right
   --
   --               __________  max_value
   --              /
   --             /
   --   _________/
   --           H  M

   ---------------
   -- Variables --
   ---------------

   type Variable is abstract tagged private;

   procedure Set_Name (Self : in out Variable; Name : String);
   function Get_Name (Self : Variable) return String;
   --  Set the name of the variable.

   procedure Set_Range (Self : in out Variable; Min, Max : Scalar);
   --  Set the range of valid values for the variable. By default, this
   --  is the whole set of values allowed for Scalar.
   --  Setting this range is important in most cases since it impacts the
   --  computation for the defuzzification methods (a centroid requires
   --  integration, for instance, and the precision will be much better on
   --  a smaller range). The range is also mandatory when using slope
   --  membership functions (since otherwise the center of gravity will
   --  always end up as 0.0 or 1.0).

   procedure Add_Term
      (Self       : in out Variable;
       Name       : String;
       Membership : not null access Membership_Function'Class)
       with Pre => not Self.Is_Frozen;
   --  Add a new linguistic term for the variable.
   --  If the variable is "Outside_Temperature", for instance, one of the
   --  terms could be "Hot", or "Cold".

   function Is_Frozen (Self : Variable'Class) return Boolean
      with Inline;
   --  Whether new terms can be added to the variable.
   --  This is no longer true once the variable has been added to an engine,
   --  and some rules were also added to the engine.

   procedure Set_Value (Self : in out Variable; Value : Scalar);
   function Get_Value (Self : Variable) return Scalar;
   --  Return the current value of the variable.
   --  Setting the value does not automatically cause a recomputation of the
   --  output variables.

   type Input_Variable is new Variable with private;
   type Output_Variable is new Variable with private;

   ------------
   -- Hedges --
   ------------
   --  These functions can be used to alter the meaning of terms and
   --  membership functions. For instance:
   --     R := Implies (Temp = Very ("COLD"), Thermostat = not "SLOW");

   type Hedge_Func is access function (X : Membership) return Membership;
   type Term_With_Hedge is record
      Term : Ada.Strings.Unbounded.Unbounded_String;
      Func : Hedge_Func;
   end record;

   function "not" (Term : String) return Term_With_Hedge;
   function Very (Term : String) return Term_With_Hedge;

   -----------
   -- Rules --
   -----------
   --  Rules ("if ... then ...") are grouped into rule blocks. This is a
   --  convenience that allows quickly disabling set of rules depending on
   --  the various states of the system, for instance.

   type Rule is private;
   type Rule_Array is array (Positive range <>) of Rule;

   type Input_Expr is private;
   type Output_Expr is private;

   function "="
      (Var       : not null access Input_Variable'Class;
       Term      : String) return Input_Expr;
   function "="
      (Var       : not null access Input_Variable'Class;
       Term      : Term_With_Hedge) return Input_Expr;
   function "="
      (Var       : not null access Output_Variable'Class;
       Term      : String) return Output_Expr;
   --  Create a new expression testing the membership of a specific
   --  variable.
   --  The variable must have been added to the engine already. The rule
   --  is only usable while the variable is defined, i.e. while the engine
   --  that owns it exists.
   --  The name of the terms is case sensitive, so they must be speciifed
   --  exactly as when they were created.

   type Input_Expr_Array is array (Positive range <>) of Input_Expr;
   type Output_Expr_Array is array (Positive range <>) of Output_Expr;

   function Implies
      (Input  : Input_Expr_Array;
       Output : Output_Expr_Array;
       Weight : Membership := 1.0) return Rule;
   function Implies
      (Input  : Input_Expr;
       Output : Output_Expr_Array;
       Weight : Membership := 1.0) return Rule;
   function Implies
      (Input  : Input_Expr;
       Output : Output_Expr;
       Weight : Membership := 1.0) return Rule;
   function Implies
      (Input  : Input_Expr_Array;
       Output : Output_Expr;
       Weight : Membership := 1.0) return Rule;
   --  Builds a rule. For instance
   --     if Temp is COLD and Time is DAY then Thermostat is FULL_POWER;
   --  can be written as:
   --     R := Implies
   --        ((Temp = "COLD",  Time = "DAY"),
   --         Thermostat = "FULL_POWER");
   --
   --  It is invalid to have the same variable referenced twice in either
   --  Input or Output.
   --  This representation only supports the "AND" operator, so a rule
   --  using "OR" must be decomposed into a set of equivalent rules, as in:
   --       if A is "A1"   OR    B is "B1"   THEN   C is "C1"
   --    <=>
   --       if A is "A1"   THEN   C is "C1"
   --       if B is "B1"   THEN   C is "C1"

   ---------------------
   -- Fuzzy Operators --
   ---------------------
   --  The fuzzy operators play a similar role to the standard logical
   --  operators, applied to fuzzy values. They have the following
   --  requirements:
   --     * must lead the same result as standard operators on crisp
   --       values (0.0 and 1.0)
   --
   --  However, they can be defined multiple ways. Each of the following
   --  definitions are pairs that match each other.

   type Operator is access function (V1, V2 : Membership) return Membership;

   function And_As_Min (V1, V2 : Membership) return Membership
      is (Membership'Min (V1, V2)) with Inline;
   function Or_As_Max  (V1, V2 : Membership) return Membership
      is (Membership'Max (V1, V2)) with Inline;
   --  V1 AND V2   is implemented as   min (V1, V2)
   --  V1 OR V2    is implemented as   max (V1, V2);

   function And_As_Multiply (V1, V2 : Membership) return Membership
      is (V1 * V2) with Inline;
   function Or_As_Sum      (V1, V2 : Membership) return Membership
      is (V1 + V2 - V1 * V2) with Inline;
   --  V1 AND V2   is implemented as   V1 * V2
   --  V1 OR V2    is implemented as   V1 + V2 - V1 * V2

   function And_As_Vdiff   (V1, V2 : Membership) return Membership
      is (Membership'Max (0.0, V1 + V2 - 1.0)) with Inline;
   function Or_As_Vsum     (V1, V2 : Membership) return Membership
      is (Membership'Min (1.0, V1 + V2)) with Inline;
   --  V1 AND V2   is implemented as   max (0, V1 + V2 - 1)
   --  V1 OR V2    is implemented as   min (1, V1 + V2)

   ------------
   -- Engine --
   ------------

   type Engine is tagged private;
   --  The inference engine itself.
   --  Given some input variables, output variables and a set of rules to
   --  link them, this engine will compute the value of the output variables
   --  given the current value of the input variables.

   procedure Add_Input
      (Self : in out Engine; Var : not null access Input_Variable'Class)
      with Pre => not Self.Has_Rules;
   procedure Add_Output
      (Self : in out Engine; Var : not null access Output_Variable'Class)
      with Pre => not Self.Has_Rules;
   --  Add an input or output variable to the engine.
   --  This variable can later be retrieve by name, although it is in general
   --  simpler to keep a handle on the variable.
   --  The engine takes ownership of Var, which should therefore not be freed
   --  by the caller. Var will be freed when the engine itself is freed, so it
   --  is safe to keep a handle on the variable while the engine exists.

   procedure Add_Rule_Block
      (Self         : in out Engine;
       Name         : String;
       Rules        : Rule_Array;
       And_Operator : Operator := And_As_Min'Access;
       Activation   : Activation_Method := Activation_Min);
   --  Create a new rule block.
   --  No input or output variable can be added to the engine after this point
   --  since the rule block must know the number of variables. Likewise, all
   --  rules must be specified at creation time, and cannot be added
   --  dynamically.
   --  The rules are preprocessed for more efficiency, and then freed. So the
   --  caller must not reference them anymore. The intent is to specify them
   --  inline:
   --     Eng.Add_Rule_Block ("Rule1", (1 => Implicites (...)));
   --
   --  Raises:
   --     - Invalid_Variable: when a variable reference by a rule has not been
   --       added to the engine yet.
   --     - Invalid_Term: when a linguistic term is unknown for the
   --       corresponding variable

   Invalid_Variable : exception;
   Invalid_Term     : exception;

   function Has_Rules (Self : Engine'Class) return Boolean
      with Inline;
   --  Whether at least one rule was defined for the engine.
   --  When this is True, no more variables or terms can be added.

   procedure Set_Accumulation
      (Self : in out Engine; Method : Accumulation_Method := Accumulation_Max);
   --  Set the accumulation method for this engine.
   --  This is sometimes also called the Aggregation method.

   procedure Set_Defuzzification
      (Self   : in out Engine;
       Method : Defuzzification_Method := Defuzzify_Centroid);
   --  Set the method to use to convert a fuzzy set to a single scalar value

   procedure Process (Self : Engine)
      with Pre => Self.Has_Rules;
   --  Compute the value of the output variables given the current value of
   --  the input variables.
   --  Self is unmodified, only the output variables's values are modified

private

   type Membership_Function is abstract tagged null record;
   type Rule_Idx is new Positive;

   type Membership_Function_Access is access all Membership_Function'Class;
   type Membership_Function_Array is
      array (Rule_Idx range <>) of Membership_Function_Access;
   --  Must be freed by the record that contains this type

   type Fuzzy_Var_Idx is new Positive;
   type Fuzzy_Var_Values is array (Fuzzy_Var_Idx range <>) of Membership;
   type Fuzzy_Var_Values_Access is access all Fuzzy_Var_Values;
   Term_Not_Found : constant Fuzzy_Var_Idx := Fuzzy_Var_Idx'Last;
   --  For each var and for each term, computes the membership

   type Term is record
      Name       : Unbounded_String;
      Membership : access Membership_Function'Class;

      In_Rules   : Natural := 0;
      --  Number of rules that this term is needed for. A term that is not
      --  needed by any rule does not need to be fuzzified.

      Idx        : Fuzzy_Var_Idx;
      --  The index in the engine for this term combination (computed for
      --  the associated variable).
   end record;

   package Term_Vectors is new Ada.Containers.Vectors (Positive, Term);

   type Variable is abstract tagged record
      Name     : Unbounded_String;
      Min      : Scalar := Scalar'First;
      Max      : Scalar := Scalar'Last;
      Value    : Scalar := 0.0;
      Terms    : Term_Vectors.Vector;
      Frozen   : Boolean := False;
   end record;

   type Input_Variable is new Variable with null record;
   type Input_Variable_Access is access all Input_Variable'Class;

   type Output_Variable is new Variable with null record;
   type Output_Variable_Access is access all Output_Variable'Class;

   type Input_Expr is record
      Var  : not null access Input_Variable'Class;
      Term : Term_With_Hedge;
   end record;

   type Output_Expr is record
      Var  : not null access Output_Variable'Class;
      Term : Unbounded_String;
   end record;

   type Input_Expr_Array_Access is access Input_Expr_Array;
   type Output_Expr_Array_Access is access Output_Expr_Array;

   type Rule is record
      Input  : Input_Expr_Array_Access;
      Output : Output_Expr_Array_Access;
      Weight : Membership;
   end record;

   type Var_Idx is new Positive;
   package Input_Variable_Vectors is new Ada.Containers.Vectors
      (Var_Idx, Input_Variable_Access);
   package Output_Variable_Vectors is new Ada.Containers.Vectors
      (Var_Idx, Output_Variable_Access);

   type Rule_Weights is array (Rule_Idx range <>) of Membership;

   type Antecedent_Details is record
      Fuzzy_Value : Fuzzy_Var_Idx := Term_Not_Found;
      Hedge       : Hedge_Func := null;
   end record;
   type Antecedents is array (Rule_Idx range <>, Var_Idx range <>)
      of Antecedent_Details;
   --  Which fuzzy value to use for the each variable.

   type Consequent_Details is record
      Term : Membership_Function_Access;   --  owned
   end record;
   type Consequents is array (Rule_Idx range <>, Var_Idx range <>)
      of Consequent_Details;

   type Rule_Block_Details;
   type Rule_Block is access all Rule_Block_Details;
   type Rule_Block_Details
      (Rules_Count       : Rule_Idx;
       Input_Vars_Count  : Var_Idx;
       Output_Vars_Count : Var_Idx) is
   record
      Name    : Unbounded_String;

      Enabled : Boolean := True;
      --  Whether this rule block should be computed

      And_Operator : Operator;
      Activation   : Activation_Method;

      Weights : Rule_Weights (1 .. Rules_Count);
      Left    : Antecedents (1 .. Rules_Count, 1 .. Input_Vars_Count);
      Right   : Consequents (1 .. Rules_Count, 1 .. Output_Vars_Count);

      Next    : Rule_Block;
   end record;
   --  A rule block is a set of rules, represented in a memory efficient
   --  manner to avoid double computations and allocating too much
   --  memory. It is represented as a matrix:
   --          | Input1  | Input2  || Output1 | Output2 |
   --          |---------+---------++---------+---------|
   --    Rule1 | Term1.1 | Term2.1 || Term3.1 | Term4.1 |
   --    Rule2 | Term1.2 | Term2.2 || Term3.2 | Term4.2 |
   --
   --  where each of the "Term" is an index into the Vars array. This
   --  array contains the computed fuzzy values for the variables for each
   --  term. Each term also contains an optional Hedge.

   type Engine is tagged record
      Inputs  : Input_Variable_Vectors.Vector;
      Outputs : Output_Variable_Vectors.Vector;
      Rules   : Rule_Block;                --  List of rule blocks
      Accumulation : Accumulation_Method := Accumulation_Max;
      Defuzzification : Defuzzification_Method := Defuzzify_Centroid;

      Total_Rules : Rule_Idx;
      --  Number of registered rules, in all rule blocks.

      Total_Input_And_Terms : Fuzzy_Var_Idx;
      --  Number of input variables and their terms
   end record;

end Fuzzy;
