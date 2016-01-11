with Ada.Exceptions;    use Ada.Exceptions;
with Fuzzy;
with Fuzzy.FCL;
with GNATCOLL.Traces;   use GNATCOLL.Traces;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with Ada.Text_IO;       use Ada.Text_IO;

procedure Robot is
   package Fuzzys is new Fuzzy (Scalar => Float, Base_Membership => Float);
   package Fuzzys_FCL is new Fuzzys.FCL;
   use Fuzzys, Fuzzys_FCL;

   Eng         : Engine;
begin
   GNATCOLL.Traces.Parse_Config_File;

   Eng := Parse_FCL_File (Create ("robot.fcl"));

   Free (Eng);
   GNATCOLL.Traces.Finalize;

exception
   when E : Parse_Error =>
      Put_Line (Exception_Message (E));
end Robot;
