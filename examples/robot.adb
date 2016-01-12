with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Numerics;      use Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Fuzzy;
with Fuzzy.FCL;
with GNATCOLL.Traces;   use GNATCOLL.Traces;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with Ada.Text_IO;       use Ada.Text_IO;

with Glib;                     use Glib;
with Glib.Main;                use Glib.Main;
with Gdk.RGBA;                 use Gdk.RGBA;
with Gtk.Window;               use Gtk.Window;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Label;                use Gtk.Label;
with Gtk.Main;                 use Gtk.Main;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Table;                use Gtk.Table;
with Gtkada.Canvas_View;       use Gtkada.Canvas_View;
--  with Gtkada.Canvas_View.Views; use Gtkada.Canvas_View.Views;
with Gtkada.Style;             use Gtkada.Style;

procedure Robot is
   package Fuzzys is new Fuzzy (Scalar => Gdouble, Base_Membership => Gdouble);
   package Fuzzys_FCL is new Fuzzys.FCL;
   use Fuzzys, Fuzzys_FCL;

   Obstacle_Radius : constant Gdouble := 15.0;
   Robot_Radius    : constant Gdouble := 10.0;
   World_Width  : constant Gdouble := 40.0 * Obstacle_Radius * 2.0;
   World_Height : constant Gdouble := 23.0 * Obstacle_Radius * 2.0;

   Update_Time : constant Gdouble := 0.03;
   --  Virtual time (seconds) between two moves of the robots

   package Scalar_Numerics is new Ada.Numerics.Generic_Elementary_Functions
      (Gdouble);
   use Scalar_Numerics;

   function Normalize (A : Gdouble) return Gdouble;
   --  Return a value between -180deg and 180deg

   type Fuzzy_Engine is record
      Eng   : Engine;

      Front         : access Input_Variable'Class;
      Right         : access Input_Variable'Class;
      Left          : access Input_Variable'Class;
      Goal_Dist     : access Input_Variable'Class;
      Goal_Angle    : access Input_Variable'Class;
      --  The various sensors for a robot

      Speed         : access Output_Variable'Class;
      Angle_Speed   : access Output_Variable'Class;
      --  The result of the fuzzy engine
   end record;

   type Sensors_GUI is record
      Front, Right, Left, Goal_Dist, Goal_Angle : Gtk_Label;
      Speed, Angle_Speed : Gtk_Label;
   end record;

   procedure Add_Sensors
      (Parent  : not null access Gtk_Box_Record'Class;
       Sensors : in out Sensors_GUI);
   --  Add widgets representing the sensors to Parent

   type Robot (Var_Count : Var_Idx) is record
      Item : Ellipse_Item;
      Goal : Rect_Item;

      X, Y               : Gdouble;
      Angle              : Gdouble;
      Target_X, Target_Y : Gdouble;

      Sensors            : Sensors_GUI;

      Vars               : Variable_Values (Var_Count);
      --  Input and output variables for the fuzzy engine.
   end record;

   type World is record
      Model  : List_Canvas_Model;
      Engine : Fuzzy_Engine;
   end record;

   procedure Create_World (Self : in out World);
   --  Populate the world with obstacles

   procedure Add_Robot (Self : in out Robot; W : World; X, Y : Gdouble);
   --  Add a robot

   procedure Create_Fuzzy (Self : in out Fuzzy_Engine);
   --  Load the fuzzy engine

   procedure Move_Robot (Self : in out Robot; W : World);
   --  Move the robot one step

   procedure Find_Closest_Obstacles (Self : in out Robot; W : World);
   --  Update Front, Right and Left sensors for the robot

   W  : World;
   R  : Robot (Var_Count => 7);
   --   ??? 7 is the number of variables in the fuzzy engine

   function Animate return Boolean;
   --  Periodic function to update the state of the world

   ------------------
   -- Create_Fuzzy --
   ------------------

   procedure Create_Fuzzy (Self : in out Fuzzy_Engine) is
   begin
      Self.Eng := Parse_FCL_File (Create ("robot.fcl"));
      Self.Front      := Self.Eng.Get_Input_Variable ("Front");
      Self.Right      := Self.Eng.Get_Input_Variable ("Right");
      Self.Left       := Self.Eng.Get_Input_Variable ("Left");
      Self.Goal_Dist  := Self.Eng.Get_Input_Variable ("GoalDist");
      Self.Goal_Angle := Self.Eng.Get_Input_Variable ("GoalAngle");

      Self.Speed       := Self.Eng.Get_Output_Variable ("Speed");
      Self.Angle_Speed := Self.Eng.Get_Output_Variable ("AngleSpeed");
   end Create_Fuzzy;

   ------------------
   -- Create_World --
   ------------------

   procedure Create_World (Self : in out World) is
      Diameter : constant Gdouble := 2.0 * Obstacle_Radius;
      Filled  : Drawing_Style;

      procedure Add_Ellipse (X, Y : Gdouble);
      procedure Add_Ellipse (X, Y : Gdouble) is
         Ellipse : Ellipse_Item;
      begin
         Ellipse := Gtk_New_Ellipse (Filled, Diameter, Diameter);
         Ellipse.Set_Position ((X, Y));
         Self.Model.Add (Ellipse);
      end Add_Ellipse;

      X       : Gdouble;
   begin
      Create_Fuzzy (Self.Engine);

      Gtk_New (Self.Model);

      Filled := Gtk_New
         (Stroke  => Black_RGBA,
          Fill    => Create_Rgba_Pattern ((0.0, 0.0, 0.0, 0.2)));

      --  Borders of the world, so that the robot stays in the arena

      X := Obstacle_Radius;
      while X < World_Width loop
         Add_Ellipse (X, Obstacle_Radius);
         Add_Ellipse (X, World_Height - Obstacle_Radius);
         X := X + Diameter;
      end loop;

      X := Obstacle_Radius + Diameter;
      while X < World_Height - Obstacle_Radius loop
         Add_Ellipse (Obstacle_Radius, X);
         Add_Ellipse (World_Width - Obstacle_Radius, X);
         X := X + Diameter;
      end loop;

      --  Obstacles in the middle

      X := 400.0;
      while X < 800.0 loop
         Add_Ellipse (X, 250.0);
         Add_Ellipse (X, 450.0);
         X := X + Diameter;
      end loop;
   end Create_World;

   ---------------
   -- Add_Robot --
   ---------------

   procedure Add_Robot (Self : in out Robot; W : World; X, Y : Gdouble) is
      Filled, Line  : Drawing_Style;
      Poly    : Polyline_Item;
   begin
      Self.X             := X;
      Self.Y             := Y;
      Self.Angle         := 0.0;
      Self.Target_X      := 800.0;
      Self.Target_Y      := 600.0;

      Filled := Gtk_New
         (Stroke  => Black_RGBA,
          Fill    => Create_Rgba_Pattern ((0.0, 1.0, 1.0, 1.0)));
      Line  := Gtk_New (Stroke => Black_RGBA);

      Self.Item := Gtk_New_Ellipse
         (Filled, Robot_Radius * 2.0, Robot_Radius * 2.0);

      Poly := Gtk_New_Polyline
         (Line, ((Robot_Radius - 4.0, 6.0),
                 (Robot_Radius, 2.0),
                 (Robot_Radius + 4.0, 6.0)));
      Self.Item.Add_Child (Poly);
      Self.Item.Set_Position ((Self.X, Self.Y));
      W.Model.Add (Self.Item);

      Self.Goal := Gtk_New_Rect (Filled, 10.0, 10.0);
      Self.Goal.Set_Position ((Self.Target_X, Self.Target_Y));
      W.Model.Add (Self.Goal);

      W.Engine.Speed.Set_Value       (Self.Vars, 0.0);
      W.Engine.Angle_Speed.Set_Value (Self.Vars, 0.0);
   end Add_Robot;

   ----------------------------
   -- Find_Closest_Obstacles --
   ----------------------------

   procedure Find_Closest_Obstacles (Self : in out Robot; W : World) is
      Front_Min_Angle, Front_Max_Angle : Gdouble;
      Right_Min_Angle, Right_Max_Angle : Gdouble;
      Left_Min_Angle,  Left_Max_Angle  : Gdouble;
      Front_Min_Dist : Gdouble := Gdouble'Last;
      Right_Min_Dist : Gdouble := Gdouble'Last;
      Left_Min_Dist  : Gdouble := Gdouble'Last;
      Front_Item, Right_Item, Left_Item : Ellipse_Item;

      procedure For_Item (Item : not null access Abstract_Item_Record'Class);
      procedure For_Item
         (Item : not null access Abstract_Item_Record'Class)
      is
         P : constant Point := Item.Position;
         D, A : Gdouble;
      begin
         if Item /= Self.Item and then Item /= Self.Goal then
            --   ??? Simple minded detection for now, assume circles

            D := Sqrt ((Self.X - P.X) * (Self.X - P.X)
                       + (Self.Y - P.Y) * (Self.Y - P.Y))
               - Robot_Radius - Obstacle_Radius;

            A := -Arctan (Y => P.Y - Self.Y, X => P.X - Self.X) * 180.0 / Pi
               + Self.Angle;
            A := Normalize (A);

            if Front_Min_Angle <= A
               and then A <= Front_Max_Angle
            then
               Ellipse_Item (Item).Set_Style
                  (Gtk_New
                     (Fill    => Create_Rgba_Pattern ((1.0, 0.0, 0.0, 0.2))));

               if D <= Front_Min_Dist then
                  Front_Min_Dist := D;
                  Front_Item := Ellipse_Item (Item);
               end if;

            elsif Right_Min_Angle <= A
               and then A <= Right_Max_Angle
            then
               Ellipse_Item (Item).Set_Style
                  (Gtk_New
                     (Fill    => Create_Rgba_Pattern ((0.0, 1.0, 0.0, 0.2))));

               if D <= Right_Min_Dist then
                  Right_Min_Dist := D;
                  Right_Item := Ellipse_Item (Item);
               end if;

            elsif Left_Min_Angle <= A
               and then A <= Left_Max_Angle
            then
               Ellipse_Item (Item).Set_Style
                  (Gtk_New
                     (Fill    => Create_Rgba_Pattern ((0.0, 0.0, 1.0, 0.2))));
               if D <= Left_Min_Dist then
                  Left_Min_Dist := D;
                  Left_Item := Ellipse_Item (Item);
               end if;

            else
               Ellipse_Item (Item).Set_Style
                  (Gtk_New
                     (Fill    => Create_Rgba_Pattern ((0.0, 0.0, 0.0, 0.2))));
            end if;
         end if;
      end For_Item;

   begin
      --  Compute the sensors angle (absolute)
      Front_Min_Angle := (-20.0);
      Front_Max_Angle := (+20.0);
      Right_Min_Angle := (-90.0 - 70.0);
      Right_Max_Angle := (-90.0 + 70.0);
      Left_Min_Angle  := (+90.0 - 70.0);
      Left_Max_Angle  := (+90.0 + 70.0);

      --  ??? Could limit to an area with maximum detection area
      W.Model.For_Each_Item (For_Item'Access);

      --  Set the sensors
      W.Engine.Front.Set_Value      (Self.Vars, Front_Min_Dist);
      W.Engine.Right.Set_Value      (Self.Vars, Right_Min_Dist);
      W.Engine.Left.Set_Value       (Self.Vars, Left_Min_Dist);

      Self.Sensors.Front.Set_Text (Front_Min_Dist'Img);
      Self.Sensors.Right.Set_Text (Right_Min_Dist'Img);
      Self.Sensors.Left.Set_Text  (Left_Min_Dist'Img);

      if Front_Item /= null then
         Front_Item.Set_Style
            (Gtk_New
               (Fill    => Create_Rgba_Pattern ((1.0, 0.0, 0.0, 0.9))));
      end if;
      if Right_Item /= null then
         Right_Item.Set_Style
            (Gtk_New
               (Fill    => Create_Rgba_Pattern ((0.0, 1.0, 0.0, 0.9))));
      end if;
      if Left_Item /= null then
         Left_Item.Set_Style
            (Gtk_New
               (Fill    => Create_Rgba_Pattern ((0.0, 0.0, 1.0, 0.9))));
      end if;
   end Find_Closest_Obstacles;

   ---------------
   -- Normalize --
   ---------------

   function Normalize (A : Gdouble) return Gdouble is
      R : Gdouble := A;
   begin
      while R < -180.0 loop
         R := R + 360.0;
      end loop;

      while R > 180.0 loop
         R := R - 360.0;
      end loop;
      return R;
   end Normalize;

   ----------------
   -- Move_Robot --
   ----------------

   procedure Move_Robot (Self : in out Robot; W : World) is
      S, AS : Gdouble;
      Tmp   : Gdouble;
   begin
      --  Compute the sensors
      Find_Closest_Obstacles (Self, W);

      --  Compute distance to goal

      Tmp := Sqrt
         ((Self.X - Self.Target_X) * (Self.X - Self.Target_X)
          + (Self.Y - Self.Target_Y) * (Self.Y - Self.Target_Y));
      W.Engine.Goal_Dist.Set_Value  (Self.Vars, Tmp);
      Self.Sensors.Goal_Dist.Set_Text (Tmp'Img);

      --  Compute angle with goal

      Tmp := Arctan (Y => Self.Target_Y - Self.Y,
                     X => Self.Target_X - Self.X) * 180.0 / Pi - Self.Angle;
      Tmp := Normalize (Tmp);
      W.Engine.Goal_Angle.Set_Value (Self.Vars, Tmp);
      Self.Sensors.Goal_Angle.Set_Text (Tmp'Img);

      --  Compute the output
      W.Engine.Eng.Process (Self.Vars);

      S  := W.Engine.Speed.Get_Value (Self.Vars);
      Self.Sensors.Speed.Set_Text (S'Img);

      AS := W.Engine.Angle_Speed.Get_Value (Self.Vars);
      Self.Sensors.Angle_Speed.Set_Text (AS'Img);

      --  Move the robot

      Self.Angle := Normalize (Self.Angle + AS * Update_Time);
      Self.X := Self.X + S * Cos (Self.Angle * Pi / 180.0) * Update_Time;
      Self.Y := Self.Y + S * Sin (Self.Angle * Pi / 180.0) * Update_Time;

      --  Display the robot
      Self.Item.Set_Position ((Self.X, Self.Y));
   end Move_Robot;

   -------------
   -- Animate --
   -------------

   function Animate return Boolean is
   begin
      Move_Robot (R, W);
      W.Model.Refresh_Layout;
      return True;   --  call again later
   end Animate;

   -----------------
   -- Add_Sensors --
   -----------------

   procedure Add_Sensors
      (Parent  : not null access Gtk_Box_Record'Class;
       Sensors : in out Sensors_GUI)
   is
      function Create_Sensor
         (Name : String; Display : out Gtk_Label) return Gtk_Box;

      function Create_Sensor
         (Name : String; Display : out Gtk_Label) return Gtk_Box
      is
         B : Gtk_Box;
         Label : Gtk_Label;
      begin
         Gtk_New_Hbox (B, Homogeneous => False);
         Gtk_New (Label, Name & ": ");
         B.Pack_Start (Label);
         Gtk_New (Display, "0.0");
         B.Pack_Start (Display);
         return B;
      end Create_Sensor;

      Table     : Gtk_Table;
   begin
      Gtk_New (Table, Rows => 5, Columns => 2, Homogeneous => True);
      Parent.Pack_Start (Table, Expand => False, Fill => False);

      Table.Attach (Create_Sensor ("Front", Sensors.Front), 0, 1, 0, 1);
      Table.Attach (Create_Sensor ("Left",  Sensors.Left),  0, 1, 1, 2);
      Table.Attach (Create_Sensor ("Right", Sensors.Right), 0, 1, 2, 3);
      Table.Attach
         (Create_Sensor ("Goal Dist", Sensors.Goal_Dist), 0, 1, 3, 4);
      Table.Attach
         (Create_Sensor ("Goal Angle", Sensors.Goal_Angle), 0, 1, 4, 5);

      Table.Attach (Create_Sensor ("Speed", Sensors.Speed), 1, 2, 0, 1);
      Table.Attach
         (Create_Sensor ("Angle Speed",  Sensors.Angle_Speed),  1, 2, 1, 2);
   end Add_Sensors;

   Win         : Gtk_Window;
   Canvas      : Canvas_View;
   Scrolled    : Gtk_Scrolled_Window;
   Box         : Gtk_Box;
   Id          : G_Source_Id;
   pragma Unreferenced (Id);

begin
   GNATCOLL.Traces.Parse_Config_File;

   Gtk.Main.Init;
   Gtk_New (Win, Window_Toplevel);
   Win.Set_Default_Size (1280, 800);

   Gtk_New_Vbox (Box, Homogeneous => False);
   Win.Add (Box);

   Gtk_New (Scrolled);
   Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
   Box.Pack_Start (Scrolled);

   Create_World (W);

   Gtk_New (Canvas, W.Model);
   Scrolled.Add (Canvas);

   Canvas.Scale_To_Fit;

   Add_Robot (R, W, 200.0, 200.0);
   Add_Sensors (Box, R.Sensors);

   Id := Glib.Main.Timeout_Add
      (Guint (Update_Time * 1000.0),   --  in milliseconds
       Animate'Unrestricted_Access);

   Win.Show_All;
   Gtk.Main.Main;

   Free (W.Engine.Eng);
   GNATCOLL.Traces.Finalize;

exception
   when E : Parse_Error =>
      Put_Line (Exception_Message (E));
end Robot;
