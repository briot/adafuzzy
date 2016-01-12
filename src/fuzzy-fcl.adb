with Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with GNATCOLL.Utils;  use GNATCOLL.Utils;
with GNAT.Strings;    use GNAT.Strings;

package body Fuzzy.FCL is

   package Scalar_IO is new Ada.Text_IO.Float_IO (Scalar);

   procedure Parse_Membership
      (Str    : String;
       Index  : in out Integer;
       Set    : out Membership_Function_Access);
   --  Parse a set definition from a string.

   procedure Read_Char
      (Str : String; Index : in out Positive; Char : Character);
   procedure Read_Word (Str : String; Index : in out Positive; Word : String);
   --  Skip whitespaces, and check that the next token is the given character
   --  or word. Move Index after that character or word.

   procedure Read_Variable
      (Eng   : Engine'Class;
       Str   : String;
       Index : in out Positive;
       Var   : out Variable_Access;
       Input : Boolean);
   --  Read a variable name and get the corresponding variable.
   --  An error is reported if no such variable exists. On exit, Index is left
   --  after the name of the variable.
   --  Input indicates whether we expect an input or output variable.

   procedure Parse_Word
     (Str : String; Start : in out Integer; Last : out Integer);
   --  Parse the word (in Str) starting at the first non-blank character after
   --  Start. On exit, Last is set to the last character of the word, and Start
   --  to the first character.
   --  Parse_Error is raised if there is no more word after Start.
   --  A word has the following definition:
   --     word                 ::= letter [ letter_or_underscore * ]
   --     letter               ::= 'a' .. 'z' | 'A' .. 'Z'
   --     letter_or_underscore ::= letter | '_'

   procedure Parse_Float
     (Str     : String;
      Start   : in out Integer;
      Value   : out Scalar;
      Default : Scalar := Scalar'First);
   --  Parse a float starting at Start.
   --  The result is stored in Value.
   --  If the string doesn't contain a float: if Default is not Float'First,
   --  Default is returned. Otherwise, Parse_Error is raised.
   --
   --  On exit, Start is set to the first character following the float

   procedure Skip_Comments (Str : String; Index : in out Natural);
   --  Skip whitespaces and move Index after the comment start at Str (Index),
   --  if any.

   procedure Parse_Rule
      (Eng       : Engine'Class;
       Str       : String;
       Index     : in out Natural;
       Rules     : in out Rule_Array;
       Next_Rule : in out Integer);
   --  Parse Str, assuming it contains the definition of a rule ("IF ...;")
   --  This might result in one or more rules added to Rules, starting at
   --  Next_Rule index (included).
   --  Index is left just after the last semicolon.

   procedure Parse_Fuzzify
     (Eng : in out Engine'Class; Str : String; Index : in out Positive);
   --  Parse a FUZZIFY section, and leave index after the closing
   --  END_FUZZIFY.

   procedure Parse_Defuzzify
     (Eng : in out Engine'Class; Str : String; Index : in out Positive);
   --  Parse a DEFUZZIFY section, and leave index after the closing
   --  END_DEFUZZIFY.

   procedure Parse_Ruleblock
     (Eng : in out Engine'Class; Str : String; Index : in out Positive);
   --  Parse a RULEBLOCK section, and leave index after the closing
   --  END_RULEBLOCK.

   procedure Parse_Vars
     (Eng   : in out Engine'Class;
      Str   : String;
      Index : in out Positive;
      Output_Var : Boolean);
   --  Parse a VAR_INPUT or VAR_OUTPUT section contents.
   --  Output_Var should be True when parsing output variables.
   --  The first character in the section is Index, and on exit Index
   --  is left after the END_VAR.

   ----------------
   -- Parse_Word --
   ----------------

   procedure Parse_Word
     (Str : String; Start : in out Integer; Last : out Integer) is
   begin
      Skip_Blanks (Str, Start);
      if Start > Str'Last then
         raise Parse_Error with "Expect name at " & Start'Img;
      end if;

      if not Is_Letter (Str (Start)) then
         raise Parse_Error with "Expect name at " & Start'Img;
      else
         Last := Start;
         while Last < Str'Last
           and then (Is_Letter (Str (Last + 1)) or else Str (Last + 1) = '_')
         loop
            Last := Last + 1;
         end loop;
      end if;
   end Parse_Word;

   -----------------
   -- Parse_Float --
   -----------------

   procedure Parse_Float
     (Str     : String;
      Start   : in out Integer;
      Value   : out Scalar;
      Default : Scalar := Scalar'First)
   is
   begin
      Skip_Blanks (Str, Start);

      if Start > Str'Last then
         if Default /= Scalar'First then
            Value := Default;
         else
            raise Parse_Error with "Expected float at" & Start'Img;
         end if;

      else
         Scalar_IO.Get (Str (Start .. Str'Last), Value, Last => Start);
         Start := Start + 1;
      end if;
   end Parse_Float;

   -------------------
   -- Read_Variable --
   -------------------

   procedure Read_Variable
      (Eng   : Engine'Class;
       Str   : String;
       Index : in out Positive;
       Var   : out Variable_Access;
       Input : Boolean)
   is
      Last : Positive;
   begin
      Parse_Word (Str, Index, Last);

      if Input then
         Var := Variable_Access (Eng.Get_Input_Variable (Str (Index .. Last)));
      else
         Var := Variable_Access
            (Eng.Get_Output_Variable (Str (Index .. Last)));
      end if;

      if Var = null then
         raise Parse_Error with
            "Unknown variable """ & Str (Index .. Last) & """";
      end if;

      Index := Last + 1;  --  Skip variable name
   end Read_Variable;

   -------------------
   -- Parse_Fuzzify --
   -------------------

   procedure Parse_Fuzzify
     (Eng : in out Engine'Class; Str : String; Index : in out Positive)
   is
      Name_Start, Name_Last : Natural;
      Var : Variable_Access;
      Set : Membership_Function_Access;
   begin
      Read_Variable (Eng, Str, Index, Var, Input => True);

      loop
         Skip_Comments (Str, Index);
         if Starts_With (Str (Index .. Str'Last), "END_FUZZIFY") then
            Index := Line_End (Str, Index) + 1;
            return;
         end if;

         Read_Word (Str, Index, "TERM");
         Parse_Word (Str, Index, Name_Last);
         Name_Start := Index;
         Index := Name_Last + 1;
         Read_Word (Str, Index, ":=");
         Parse_Membership (Str, Index, Set => Set);
         Read_Char (Str, Index, ';');
         Skip_Comments (Str, Index);

         Var.Add_Term
            (Name       => Str (Name_Start .. Name_Last),
             Membership => Set);
      end loop;
   end Parse_Fuzzify;

   ---------------------
   -- Parse_Defuzzify --
   ---------------------

   procedure Parse_Defuzzify
     (Eng : in out Engine'Class; Str : String; Index : in out Positive)
   is
      Name_Start, Name_Last : Natural;
      Var   : Variable_Access;
      V     : Output_Variable_Access;
      Val, Val2   : Scalar;
      Set : Membership_Function_Access;
   begin
      Read_Variable (Eng, Str, Index, Var, Input => False);
      V := Output_Variable_Access (Var);

      loop
         Skip_Comments (Str, Index);
         if Starts_With (Str (Index .. Str'Last), "END_DEFUZZIFY") then
            Index := Line_End (Str, Index) + 1;
            return;
         end if;

         Skip_Blanks (Str, Index);

         if Starts_With (Str (Index .. Str'Last), "TERM") then
            Index := Index + 4;
            Parse_Word (Str, Index, Name_Last);
            Name_Start := Index;
            Index := Name_Last + 1;
            Read_Word (Str, Index, ":=");
            Parse_Membership (Str, Index, Set => Set);
            V.Add_Term
               (Name       => Str (Name_Start .. Name_Last),
                Membership => Set);

         elsif Starts_With (Str (Index .. Str'Last), "DEFAULT") then
            Index := Index + 7;
            Read_Word (Str, Index, ":=");
            Parse_Float (Str, Index, Val);
            V.Set_Default (Val);

         elsif Starts_With (Str (Index .. Str'Last), "RANGE") then
            Index := Index + 5;
            Read_Word (Str, Index, ":=");
            Read_Char (Str, Index, '(');
            Parse_Float (Str, Index, Val);
            Read_Word (Str, Index, "..");
            Parse_Float (Str, Index, Val2);
            Read_Char (Str, Index, ')');
            V.Set_Range (Min => Val, Max => Val2);

         elsif Starts_With (Str (Index .. Str'Last), "ACCU") then
            Index := Index + 4;
            Read_Word (Str, Index, ":=");
            Parse_Word (Str, Index, Name_Last);

            if Str (Index .. Name_Last) = "MAX" then
               V.Set_Accumulation (Accumulation_Max);
            elsif Str (Index .. Name_Last) = "BSUM" then
               V.Set_Accumulation (Accumulation_Sum);
            else
               raise Parse_Error with
                  "Unknown accumulation method: "
                  & Str (Index .. Name_Last);
            end if;

            Index := Name_Last + 1;

         elsif Starts_With (Str (Index .. Str'Last), "METHOD") then
            Index := Index + 6;
            Read_Word (Str, Index, ":=");
            Parse_Word (Str, Index, Name_Last);

            if Str (Index .. Name_Last) = "COG" then
               V.Set_Defuzzification (Defuzzify_Centroid);
            else
               raise Parse_Error with
                  "Unknown defuzzification method: "
                  & Str (Index .. Name_Last);
            end if;

            Index := Name_Last + 1;

         else
            raise Parse_Error with "Unexpected keyword at" & Index'Img;
         end if;

         Read_Char (Str, Index, ';');
         Skip_Comments (Str, Index);
      end loop;
   end Parse_Defuzzify;

   ---------------------
   -- Parse_Ruleblock --
   ---------------------

   procedure Parse_Ruleblock
     (Eng : in out Engine'Class; Str : String; Index : in out Positive)
   is
      Block_Name_Start, Block_Name_End : Natural;
      Last : Natural;

      Rules : Rule_Array (1 .. 1024) := (others => <>);
      Next_Rule : Integer := Rules'First;

      Op           : Operators := Operators_Min_Max;
      Activation   : Activation_Method := Activation_Min;

   begin
      Parse_Word (Str, Index, Block_Name_End);
      Block_Name_Start := Index;
      Index := Block_Name_End + 1;

      loop
         Skip_Blanks (Str, Index);
         Skip_Comments (Str, Index);

         if Starts_With (Str (Index .. Str'Last), "END_RULEBLOCK") then
            Index := Line_End (Str, Index) + 1;
            Eng.Add_Rule_Block
               (Name         => Str (Block_Name_Start .. Block_Name_End),
                Rules        => Rules (Rules'First .. Next_Rule - 1),
                Op           => Op,
                Activation   => Activation);
            return;

         elsif Starts_With (Str (Index .. Str'Last), "AND") then
            Index := Index + 3;
            Read_Char (Str, Index, ':');
            Parse_Word (Str, Index, Last);
            if Str (Index .. Last) = "MIN" then
               Op := Operators_Min_Max;
            elsif Str (Index .. Last) = "PROD" then
               Op := Operators_Multiply;
            elsif Str (Index .. Last) = "BDIF" then
               Op := Operators_Diff;
            else
               raise Parse_Error with
                  "Invalid AND operator """ & Str (Index .. Last) & '"';
            end if;
            Read_Char (Str, Index, ';');

         elsif Starts_With (Str (Index .. Str'Last), "ACT") then
            Index := Index + 3;
            Read_Char (Str, Index, ':');
            Parse_Word (Str, Index, Last);
            if Str (Index .. Last) = "MIN" then
               Activation := Activation_Min;
            elsif Str (Index .. Last) = "PROD" then
               Activation := Activation_Prod;
            else
               raise Parse_Error with
                  "Invalid activation method """ & Str (Index .. Last) & '"';
            end if;
            Read_Char (Str, Index, ';');

         elsif Starts_With (Str (Index .. Str'Last), "RULE ") then
            Index := Index + 4;
            Skip_Blanks (Str, Index);
            while Index <= Str'Last and then Is_Digit (Str (Index)) loop
               Index := Index + 1;
            end loop;
            Read_Char (Str, Index, ':');
            Parse_Rule (Eng, Str, Index, Rules, Next_Rule);

         else
            raise Parse_Error with "Unexpected keyword at" & Index'Img;
         end if;
      end loop;
   end Parse_Ruleblock;

   ----------------
   -- Parse_Rule --
   ----------------

   procedure Parse_Rule
      (Eng       : Engine'Class;
       Str       : String;
       Index     : in out Natural;
       Rules     : in out Rule_Array;
       Next_Rule : in out Integer)
   is
      function Read_Input_Expr return Input_Expr;
      function Read_Output_Expr return Output_Expr;
      --  Read an input expression or output expression from Str, and
      --  move Index accordingly.

      ---------------------
      -- Read_Input_Expr --
      ---------------------

      function Read_Input_Expr return Input_Expr is
         Var  : Variable_Access;
         Term : Term_With_Hedge;
         Last : Integer;
      begin
         Read_Variable (Eng, Str, Index, Var, Input => True);
         Read_Word (Str, Index, "IS");
         Skip_Blanks (Str, Index);

         if Starts_With (Str (Index .. Str'Last), "VERY ") then
            Index := Index + 5;
            Parse_Word (Str, Index, Last);
            Term := Very (Str (Index .. Last));
            Index := Last + 1;
         elsif Starts_With (Str (Index .. Str'Last), "NOT ") then
            Index := Index + 4;
            Parse_Word (Str, Index, Last);
            Term := not Str (Index .. Last);
            Index := Last + 1;
         else
            Parse_Word (Str, Index, Last);
            Term.Term := To_Unbounded_String (Str (Index .. Last));
         end if;

         Index := Last + 1;
         return Input_Variable_Access (Var) = Term;
      end Read_Input_Expr;

      ----------------------
      -- Read_Output_Expr --
      ----------------------

      function Read_Output_Expr return Output_Expr is
         Var  : Variable_Access;
         Start, Last : Integer;
      begin
         Read_Variable (Eng, Str, Index, Var, Input => False);
         Read_Word (Str, Index, "IS");
         Parse_Word (Str, Index, Last);
         Start := Index;
         Index := Last + 1;
         return Output_Variable_Access (Var) = Str (Start .. Last);
      end Read_Output_Expr;

      Inputs : Input_Expr_Array (1 .. 200);
      Next_Input : Integer := Inputs'First;

      Outputs : Output_Expr_Array (1 .. 200);
      Next_Output : Integer := Outputs'First;

      Weight : Scalar := 1.0;

   begin
      Read_Word (Str, Index, "IF");

      loop
         Inputs (Next_Input) := Read_Input_Expr;
         Next_Input := Next_Input + 1;

         Skip_Blanks (Str, Index);

         if Starts_With (Str (Index .. Str'Last), "OR") then
            raise Parse_Error with
               "OR operator not supported, replace with multiple rules";

         elsif Starts_With (Str (Index .. Str'Last), "AND") then
            Index := Index + 3;

         elsif Starts_With (Str (Index .. Str'Last), "THEN") then
            Index := Index + 4;
            exit;

         else
            raise Parse_Error with
               "Unexpected keyword at" & Index'Img;
         end if;
      end loop;

      loop
         Outputs (Next_Output) := Read_Output_Expr;
         Next_Output := Next_Output + 1;

         Skip_Blanks (Str, Index);

         if Str (Index) = ';' then
            Index := Index + 1;  --  skip semicolon
            exit;

         elsif Starts_With (Str (Index .. Str'Last), "WITH") then
            Index := Index + 4;
            Parse_Float (Str, Index, Weight);
            if Weight < 0.0 or else Weight > 1.0 then
               raise Parse_Error with
                  "Weight must be in 0.0 .. 1.0";
            end if;

         elsif Starts_With (Str (Index .. Str'Last), "OR") then
            raise Parse_Error with
               "OR operator not supported, replace with multiple rules";

         elsif Starts_With (Str (Index .. Str'Last), "AND") then
            Index := Index + 3;

         else
            raise Parse_Error with
               "Unexpected keyword at" & Index'Img;
         end if;
      end loop;

      Rules (Next_Rule) := Implies
         (Input  => Inputs (Inputs'First .. Next_Input - 1),
          Output => Outputs (Outputs'First .. Next_Output - 1),
          Weight => Membership (Weight));
      Next_Rule := Next_Rule + 1;
   end Parse_Rule;

   ---------------
   -- Read_Char --
   ---------------

   procedure Read_Char
      (Str : String; Index : in out Positive; Char : Character) is
   begin
      Skip_Blanks (Str, Index);

      if Index > Str'Last or else Str (Index) /= Char then
         raise Parse_Error with "Expected '" & Char & "' at" & Index'Img;
      end if;

      Index := Index + 1;  --  skip the character
   end Read_Char;

   ---------------
   -- Read_Word --
   ---------------

   procedure Read_Word
      (Str : String; Index : in out Positive; Word : String) is
   begin
      Skip_Blanks (Str, Index);

      if Index > Str'Last
         or else not Starts_With (Str (Index .. Str'Last), Word)
      then
         raise Parse_Error with "Expected """ & Word & """ at"
            & Index'Img;
      end if;

      Index := Index + Word'Length;  --  skip the word
   end Read_Word;

   ----------------
   -- Parse_Vars --
   ----------------

   procedure Parse_Vars
     (Eng   : in out Engine'Class;
      Str   : String;
      Index : in out Positive;
      Output_Var : Boolean)
   is
      Word_Start, Word_Last : Natural;
      Var       : Variable_Access;
   begin
      loop
         Skip_Comments (Str, Index);
         Skip_Blanks (Str, Index);

         if Starts_With (Str (Index .. Str'Last), "END_VAR") then
            Index := Line_End (Str, Index) + 1;
            return;
         end if;

         Parse_Word (Str, Index, Word_Last);
         Word_Start := Index;
         Index      := Word_Last + 1;

         Read_Char (Str, Index, ':');
         Read_Word (Str, Index, "REAL");
         Read_Char (Str, Index, ';');
         Skip_Comments (Str, Index);

         if Output_Var then
            Var := new Output_Variable;
            Eng.Add_Output (Output_Variable_Access (Var));
         else
            Var := new Input_Variable;
            Eng.Add_Input (Input_Variable_Access (Var));
         end if;

         Var.Set_Name (Str (Word_Start .. Word_Last));
      end loop;
   end Parse_Vars;

   ----------------------
   -- Parse_Membership --
   ----------------------

   procedure Parse_Membership
      (Str     : String;
       Index   : in out Integer;
       Set     : out Membership_Function_Access)
   is
      type Point is record
         X : Scalar;
         Y : Membership;
      end record;

      Points    : array (1 .. 4) of Point;
      Point_Num : Natural := Points'First;
      Val       : Scalar;
   begin
      loop
         Skip_Blanks (Str, Index);
         exit when Str (Index) = ';';

         Read_Char (Str, Index, '(');

         if Point_Num > Points'Last then
            raise Parse_Error with
               "Too many points defined at" & Index'Img;
         end if;

         Parse_Float (Str, Index, Points (Point_Num).X);
         Read_Char (Str, Index, ',');
         Parse_Float (Str, Index, Val);
         Read_Char (Str, Index, ')');

         if Val < 0.0 or else Val > 1.0 then
            raise Parse_Error with
               "Second value must be in range 0..1, got"
               & Val'Img & " at" & Index'Img;
         end if;

         Points (Point_Num).Y := Membership (Val);
         Point_Num := Point_Num + 1;
      end loop;

      if Point_Num = 2 then
         raise Parse_Error with
            "Singletons not handled in this version at" & Index'Img;

      elsif Point_Num = 3 then
         if Points (Points'First).Y < Points (Points'First + 1).Y then
            if Points (Points'First).Y /= 0.0 then
               raise Parse_Error with
                   "Ramps must start at 0.0 at" & Index'Img;
            end if;

            Set := Create_Ramp_Right
              (Higher    => Points (Points'First).X,
               Max       => Points (Points'First + 1).X,
               Max_Value => Points (Points'First + 1).Y);

         else
            if Points (Points'First + 1).Y /= 0.0 then
               raise Parse_Error with
                   "Ramps must end at 0.0 at" & Index'Img;
            end if;

            Set := Create_Ramp_Left
              (Max       => Points (Points'First).X,
               Lower     => Points (Points'First + 1).X,
               Max_Value => Points (Points'First).Y);
         end if;

      elsif Point_Num = 4 then
         if Points (Points'First).Y /= 0.0
           or else Points (Points'First + 2).Y /= 0.0
         then
            raise Parse_Error with
               "Triangles must start and end at 0.0, at" & Index'Img;
         end if;

         Set := Create_Triangle
           (Lower     => Points (Points'First).X,
            Max       => Points (Points'First + 1).X,
            Higher    => Points (Points'First + 2).X,
            Max_Value => Points (Points'First + 1).Y);

      elsif Point_Num = 5 then
         if Points (Points'First).Y /= 0.0
           or else Points (Points'First + 3).Y /= 0.0
         then
            raise Parse_Error with
               "Trapezes must start and end at 0.0, at" & Index'Img;
         end if;

         if Points (Points'First + 1).Y /= Points (Points'First + 2).Y then
            raise Parse_Error with
               "Both middle points must have same membership at" & Index'Img;
         end if;

         Set := Create_Trapeze
           (Lower     => Points (Points'First).X,
            Max1      => Points (Points'First + 1).X,
            Max2      => Points (Points'First + 2).X,
            Higher    => Points (Points'First + 3).X,
            Max_Value => Points (Points'First + 1).Y);

      else
         raise Parse_Error with "Invalid membership set at" & Index'Img;
      end if;
   end Parse_Membership;

   -------------------
   -- Skip_Comments --
   -------------------

   procedure Skip_Comments (Str : String; Index : in out Natural) is
   begin
      Skip_Blanks (Str, Index);
      if Index < Str'Last and then Str (Index .. Index + 1) = "(*" then
         while Index < Str'Last
           and then (Str (Index) /= '*'
                    or else Str (Index + 1) /= ')')
         loop
            Index := Index + 1;
         end loop;

         Index := Index + 2;
         Skip_Blanks (Str, Index);
      end if;
   end Skip_Comments;

   --------------------
   -- Parse_FCL_File --
   --------------------

   function Parse_FCL_File (File : Virtual_File) return Engine is
      Eng : Engine;
      Str : GNAT.Strings.String_Access;
      Start, Word_Last, Last : Integer;
   begin
      if not File.Is_Regular_File then
         raise Parse_Error
            with "File not found: " & File.Display_Full_Name;
      end if;

      Str := File.Read_File;
      Start := Str'First;

      loop
         Skip_Blanks (Str.all, Start);
         exit when Start > Str'Last;

         Skip_Comments (Str.all, Start);

         Parse_Word (Str.all, Start, Word_Last);
         Last := Word_Last + 1;

         if Starts_With (Str (Start .. Str'Last), "FUNCTION_BLOCK ") then
            Start := Line_End (Str.all, Start) + 1;
         elsif Starts_With (Str (Start .. Str'Last), "VAR_INPUT") then
            Start := Start + 9;
            Parse_Vars (Eng, Str.all, Start, Output_Var => False);
         elsif Starts_With (Str (Start .. Str'Last), "VAR_OUTPUT") then
            Start := Start + 10;
            Parse_Vars (Eng, Str.all, Start, Output_Var => True);
         elsif Starts_With (Str (Start .. Str'Last), "FUZZIFY ") then
            Start := Start + 7;
            Parse_Fuzzify (Eng, Str.all, Start);
         elsif Starts_With (Str (Start .. Str'Last), "DEFUZZIFY ") then
            Start := Start + 10;
            Parse_Defuzzify (Eng, Str.all, Start);
         elsif Starts_With (Str (Start .. Str'Last), "RULEBLOCK") then
            Start := Start + 9;
            Parse_Ruleblock (Eng, Str.all, Start);
         elsif Starts_With
            (Str (Start .. Str'Last), "END_FUNCTION_BLOCK")
         then
            Start := Line_End (Str.all, Start) + 1;
         else
            raise Parse_Error with
               "Unexpected word: " & Str (Start .. Last - 1) & " at"
               & Start'Img;
         end if;
      end loop;

      GNAT.Strings.Free (Str);

      return Eng;

   exception
      when others =>
         GNAT.Strings.Free (Str);
         raise;
   end Parse_FCL_File;

end Fuzzy.FCL;
