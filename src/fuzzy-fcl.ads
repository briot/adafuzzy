
--  This package provides a parser for the Fuzzy Control Language (IEC 1131-7)
--  A free draft version of the standard is available at:
--     http://www.fuzzytech.com/binaries/ieccd1.pdf

with GNATCOLL.VFS;           use GNATCOLL.VFS;

generic
package Fuzzy.FCL is

   function Parse_FCL_File
      (File : GNATCOLL.VFS.Virtual_File) return Engine;
   --  Parse a file written with the FCL.
   --  See the link at the start of this package for more information on
   --  this format.
   --  This is an alternative to creating the engine programmatically.
   --  No variable can be added to the resulting engine, although new rule
   --  blocks can be added programmatically.
   --
   --  Grammar for the FCL files (approximative BNF):
   --
   --    name ::= [a-zA-Z][_a-zA-Z0-9]*
   --    comment ::=  "(*" .* "*)"
   --    file ::= "FUNCTION_BLOCK" name "\n"
   --               function_block
   --             "END_FUNCTION_BLOCK"
   --    function_block ::=
   --         var_input | var_output | fuzzify | defuzzify | ruleblock
   --    var_input ::=
   --             "VAR_INPUT" "\n"
   --                 var_def*
   --             "END_VAR"
   --    var_output ::=
   --             "VAR_OUTPUT" "\n"
   --                 var_def*
   --             "END_VAR"
   --    var_def  ::= name ":" "REAL" ";"
   --    fuzzify  ::=
   --             "FUZZIFY" name
   --               term*
   --             "END_FUZZIFY"
   --    term     ::= "TERM" name ":=" point+ ";"
   --    point    ::= "(" float "," float ")"
   --    defuzzify  ::=
   --             "DEFUZZIFY" name
   --               term*
   --               ( accu | defuzzify_method | default | range )*
   --             "END_DEFUZZIFY"
   --    accu     ::=  "ACCU" ":=" ( "MAX" | "BSUM" ) ";"
   --    defuzzify_method ::= "METHOD" ":=" "COG" ";"
   --    default  ::= "DEFAULT" ":=" float ";"
   --    range    ::= "RANGE" ":=" "(" float ".." float ")" ";"
   --    ruleblock ::=
   --             "RULEBLOCK" name
   --                [ operator_def ]
   --                [ accumulation ]
   --                rule *
   --             "END_RULEBLOCK"
   --    operator_def ::=
   --             "AND" ":" ( "MIN" | "PROD" | "BDIF" ) ";"
   --    activation ::=
   --             "ACT" ":" ( "PROD" | "MIN" ) ";"
   --    rule ::=
   --             "RULE" int ":" "IF" condition "THEN" conclusion
   --                  [ "WITH" float ] ";"
   --
   --  Limitations
   --  * This parser assumes that all variables (the VAR_INPUT and VAR_OUTPUT
   --    sections) are found before any other section.
   --  * ACCU is not supported for rule blocks. Set it for the output
   --    variables instead.
   --
   --  Raises: this function might raise Parse_Error, when the file contains
   --     syntax errors

   Parse_Error : exception;

end Fuzzy.FCL;
