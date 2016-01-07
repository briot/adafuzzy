
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

package Fuzzy is

   type Membership is new Base_Membership range 0.0 .. 1.0;

   --------------------------
   -- Membership functions --
   --------------------------

   type Membership_Function is abstract tagged private;

   function Value
      (Self : Membership_Function; X : Scalar) return Membership is abstract;
   --  Returns the degree of membership of X to the set.

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

   procedure Add_Term
      (Self       : in out Variable;
       Name       : String;
       Membership : not null access Membership_Function'Class);
   --  Add a new linguistic term for the variable.
   --  If the variable is "Outside_Temperature", for instance, one of the
   --  terms could be "Hot", or "Cold".

   ---------------------
   -- Input_Variables --
   ---------------------

   type Input_Variable is new Variable with private;

   procedure Set_Value (Self : in out Input_Variable; Value : Scalar);
   --  Set the value of the variable.
   --  This does not automatically cause a recomputation of the output
   --  variables.

   ----------------------
   -- Output_Variables --
   ----------------------

   type Output_Variable is new Variable with private;

   function Get_Value (Self : Output_Variable) return Scalar;
   --  Return the current value of the variable.
   --  This value must be explicitly by the engine, this procedure does no
   --  recomputation.

   -----------
   -- Rules --
   -----------

   type Rule is private;
   type Base_Input_Expr is private;
   type Input_Expr is private;
   type Output_Expr is private;

   function "="
      (Var       : not null access Input_Variable'Class;
       Term      : String) return Base_Input_Expr;
   function "="
      (Var       : not null access Output_Variable'Class;
       Term      : String) return Output_Expr;
   --  Create a new expression testing the membership of a specific
   --  variable.
   --  The variable must have been added to the engine already. The rule
   --  is only usable while the variable is defined, i.e. while the engine
   --  that owns it exists.

   function "and" (Expr1, Expr2 : Input_Expr) return Input_Expr;
   function "or"  (Expr1, Expr2 : Input_Expr) return Input_Expr;
   function "not" (Expr : Input_Expr) return Input_Expr;

   function "and" (Expr1, Expr2 : Output_Expr) return Output_Expr;

   function Implies
      (Input  : Input_Expr;
       Output : Output_Expr;
       Weight : Membership := 1.0) return Rule;
   --  Builds a rule. For instance
   --     if Temp is COLD and Time is DAY then Thermostat is FULL_POWER;
   --  can be written as:
   --     R := Implies
   --        (Temp = "COLD" and Time = "DAY",
   --         Thermostat = "FULL_POWER");

   ------------
   -- Hedges --
   ------------
   --  These functions can be used to alter the meaning of terms and
   --  membership functions. For instance:
   --     R := Implies
   --        (Very (Temp = "COLD"), Very (Thermostat = "FULL_POWER"));

   function Very (Expr : Base_Input_Expr) return Input_Expr;
   function Very (Expr : Base_Output_Expr) return Output_Expr;

   -----------------
   -- Rule blocks --
   -----------------

   type Rule_Block is private;

   function Create_Rule_Block (Name : String := "") return Rule_Block;
   --  Create a new empty rule block.
   --  The name is used to later retrieve the rule block by name from the
   --  engine.

   procedure Add_Rule (Self : in out Rule_Block; R : Rule);
   --  Add a new rule to the block

   ------------
   -- Engine --
   ------------

   type Engine is tagged private;
   --  The inference engine itself.
   --  Given some input variables, output variables and a set of rules to
   --  link them, this engine will compute the value of the output variables
   --  given the current value of the input variables.

   procedure Add_Input
      (Self : in out Engine; Var : not null access Input_Variable'Class);
   procedure Add_Output
      (Self : in out Engine; Var : not null access Output_Variable'Class);
   --  Add an input or output variable to the engine.
   --  This variable can later be retrieve by name, although it is in general
   --  simpler to keep a handle on the variable.
   --  The engine takes ownership of Var, which should therefore not be freed
   --  by the caller. Var will be freed when the engine itself is freed, so it
   --  is safe to keep a handle on the variable while the engine exists.

   procedure Add_Rule_Block
      (Self : in out Engine; Rules : not null access Rule_Block'Class);
   --  Add a new rule to the engine.
   --  Rules is adopted by the engine, and must not be freed by the caller.

   procedure Process (Self : Engine);
   --  Compute the value of the output variables given the current value of
   --  the input variables. The engine itself is not modified, but the
   --  variables are modified.

private

   type Membership_Function is abstract tagged null record;

   type Term is record
      Name       : Unbounded_String;
      Membership : access Membership_Function'Class;
   end record;

   package Term_Vectors is new Ada.Containers.Vectors (Positive, Term);

   type Variable is abstract tagged record
      Name     : Unbounded_String;
      Min, Max : Scalar;
      Value    : Scalar;
      Terms    : Term_Vectors.Vector;
   end record;

   type Input_Variable is new Variable with null record;
   type Input_Variable_Access is access all Input_Variable'Class;

   type Output_Variable is new Variable with null record;
   type Output_Variable_Access is access all Output_Variable'Class;

   type Input_Expr_Kind is
      (Input_Expr_Simple,
       Input_Expr_Or,
       Input_Expr_And,
       Input_Expr_Not);

   type Input_Expr (Kind : Input_Expr_Kind := Input_Expr_Simple) is record
      case Kind is
         when Input_Expr_Simple =>
            Var : Input_Variable_Access;
         when Input_Expr_Or | Input_Expr_And =>
            Expr1, Expr2 : Input_Expr;
         when Input_Expr_Not =>
            Expr : Input_Expr;
      end case;
   end record;

   type Output_Expr is record
      Var : Output_Variable_Access;
   end record;

   type Rule is record
      Input  : Input_Expr;
      Output : Output_Expr;
   end record;

   package Input_Variable_Vectors is new Ada.Containers.Vectors
      (Positive, Input_Variable_Access);
   package Output_Variable_Vectors is new Ada.Containers.Vectors
      (Positive, Output_Variable_Access);
   package Rule_Vectors is new Ada.Containers.Vectors
      (Positive, Rule);

   type Rule_Block is tagged record
      Rules : Rule_Vectors.Vector;
   end record;
   type Rule_Block_Access is access all Rule_Block'Class;

   package Rule_Block_Vectors is new Ada.Containers.Vectors
      (Positive, Rule_Block_Access);

   type Engine is tagged record
      Inputs  : Input_Variable_Vectors.Vector;
      Outputs : Output_Variable_Vectors.Vector;
      Rules   : Rule_Block_Vectors.Vector;
   end record;

end Fuzzy;
