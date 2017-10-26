-- Name: Andrew McGuiness
-- Date: October 21, 2017
-- Course: ITEC 320 Procedural Analysis and Design
--
-- package body for the Fruit_Tree package
--
-- Purpose: The primary purpose of Fruit_Tree package is to simplify the main
--    procedure.  The program can be in one of two states, the first state is
--    an parsing mode where it builds a Tree_List, this state is handled here,
--    by the Fruit_Tree package.

package body Fruit_Tree is
   procedure Parse_Tree( tl         : in out Tree_List;
                         pos        : out Natural;
                         input_file : in out File_Type);
   procedure Parse_Fruit( c_tree : in out Tree; input_file : in out File_Type );
   
   function  Get_Fruit_Value( f : in Fruit; qual : in Fruit_Qual) return Float;
   procedure Update_Tree_Stats( t : in out Tree );
   procedure Put_Float( num : in Float );
   procedure Calculate_Average( qual : in Fruit_Qual; t : in out Tree );
   procedure Calculate_Std_Dev( qual : in Fruit_Qual; t : in out Tree );
   procedure Print_Error_Line( input_file : in File_Type);
   function  In_List(tl     : in  Tree_List;
                     pos    : out Positive;
                     c_tree : in  Tree ) return Boolean;
   
   --########################################################################--
   ----------------------- Public Subroutines ---------------------------------
   --########################################################################--
   
   
   ----------------------------------------------------------------------------
   -- Parse_Input_File --------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Parse_Fruit_File(tl : in out Tree_List ) 
   is
      KeyWord_Exception : exception;

      type    KeyWord     is (TREE, FRUIT);
      package KeyWord_IO  is new Enumeration_IO(KeyWord);

      next_key   : KeyWord;
      last_index : Natural := 0;
      input_file : File_Type;
   begin
      Open(File => input_file, Mode => In_File, Name => Argument(1));

      while not End_Of_File(input_file) loop
         -- Inner Exception Handling to enable recovery from bad input
         begin
            KeyWord_IO.Get(File => input_file, Item => next_key );

            if next_key = TREE then
               Parse_Tree(tl, last_index, input_file);

            elsif next_key = FRUIT then
               -- Ensure that the Fruit has a parent Tree
               if last_index = 0 then
                  raise Fruit_Exp;
               end if;

               Parse_Fruit( tl.trees(last_index), input_file );
            end if;

         exception
            when Fruit_Exp =>
               Print_Error_Line( input_file);
               Put_Line(": Cannot add fruit, it doesn't have a valid parent.");


            When Data_Error =>
               Print_Error_Line( input_file);
               Put_Line(": Read a non-keyword token, discarding.");


            when Fail : others =>
               Put( "Unhandled error: " );
               Put_Line( Exception_Name( Fail));
               return;
         end;
      end loop;

      Close(File => input_file);
   end Parse_Fruit_File;


   ----------------------------------------------------------------------------
   -- Equals Override ---------------------------------------------------------
   ----------------------------------------------------------------------------
   function "=" (left : Tree; right : Tree ) return Boolean
   is
      equal : Boolean := False;
   begin
      if left.id = right.id then
         equal := True;
      end if;

      return equal;
   end "=";

   
   ----------------------------------------------------------------------------
   -- Put_Tree ----------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Put_Tree( t : in Tree ) 
   is
   begin
      Put("TREE:");
      Set_Col(7);
      Put( Integer(t.id),7 );
      Set_Col(17);
      Put(t.f_count, 0);
   end Put_Tree;


   ----------------------------------------------------------------------------
   -- Get_Tree ----------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Tree( t : out Tree; input_file : in out File_Type) 
   is
      input : String(1..80);
      length: Integer;
      count : Integer;

   begin
      loop
         Get_Line(input_file, input, length);

         count := 0;
         for i in 1..length loop
            if input(i) >= '0' and input(i) <= '9' then
               count := count + 1;
            end if;
         end loop;
         exit when count > 0;
      end loop;

      t.id := ID_Number( Natural'Value(input(1..length)) );
   exception
      when others =>
         raise Tree_Exp;
   end Get_Tree;


   ----------------------------------------------------------------------------
   -- Put_Fruit ---------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Put_Fruit( f : in Fruit ) 
   is
   begin
      Set_Col(5);
      Put(f.f_firm);

      Set_Col(15);
      Put(f.f_taste);

      Set_Col(25);
      Put(f.f_size);
   end Put_Fruit;


   ----------------------------------------------------------------------------
   -- Get_Fruit ---------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Fruit( f : out Fruit; input_file : in out File_Type) 
   is
   begin
      Get( input_file, f.f_size );
      Get( input_file, f.f_firm);
      Get( input_file, f.f_taste);
   exception
      when others =>
         raise Fruit_Exp;
   end Get_Fruit;
   
   
   ----------------------------------------------------------------------------
   -- Put_Tree_Stats ----------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Put_Tree_Stats( ts : in Tree_Stat) 
   is
   begin
      Set_Col(5);
      Put_Float(ts.f_stat.average);

      Set_Col(15);
      Put_Float(ts.t_stat.average);

      Set_Col(25);
      Put_Float(ts.s_stat.average);

      New_Line;

      Set_Col(5);
      Put_Float(ts.f_stat.std_dev);

      Set_Col(15);
      Put_Float(ts.t_stat.std_dev);

      Set_Col(25);
      Put_Float(ts.s_stat.std_dev);
   end Put_Tree_Stats;
   
   
   
   --########################################################################--
   ----------------------- Private Subroutines --------------------------------
   --########################################################################--


   ----------------------------------------------------------
   -- Purpose: Print a Float according to the project specification.
   -- Parameters: num: Float to print
   ----------------------------------------------------------
   procedure Put_Float( num : in Float ) 
   is
   begin
      Put(num, Fore => 0, Aft => 1, Exp => 0);
   end Put_Float;
   
   
   ----------------------------------------------------------
   -- Purpose: Determine if a Tree is already in the Tree_List,
   --    if it is, get it's position.
   -- Parameters:    tl: Tree_List to look in for c_tree
   --               pos: position of the c_tree in tl or 0 if not found
   --            c_tree: Tree to look for
   -- Returns: True if c_tree was found inside the list, False otherwise.
   ----------------------------------------------------------
   function In_List(  tl : in Tree_List;
                      pos : out Positive;
                      c_tree : in Fruit_Tree.Tree ) return Boolean
   is
      found : Boolean := False;
   begin
      for i in 1..tl.count loop
         if c_tree = tl.trees(i) then
            pos   := i;
            found := True;
         end if;

         exit when found;
      end loop;

      return found;
   end In_List;


   ----------------------------------------------------------
   -- Purpose: Calculate the average for specified qualities based of this 
   --    Tree's fruit.
   -- Parameters: qual: Which Fruit Quality to calculate the average for
   --                t: Tree to update
   ----------------------------------------------------------
   procedure Calculate_Average( qual : in Fruit_Qual; t : in out Tree ) 
   is
      sum     : Float := 0.0;
      average : Float := 0.0;
   begin
      for i in 1..t.f_count loop
         sum := sum + Get_Fruit_Value( t.fruits(i), qual);
      end loop;

      average := sum / Float(t.f_count);

      case qual is
         when Q_SIZE =>
            t.stats.s_stat.average := average;
         when Q_FIRMNESS =>
            t.stats.f_stat.average := average;
         when Q_TASTE =>
            t.stats.t_stat.average := average;
      end case;
   end Calculate_Average;


   ----------------------------------------------------------
   -- Purpose: Calculate the standard deviation for specified qualities based 
   --    of this Tree's fruit.
   -- Parameters: qual: Which Fruit Quality to calculate the average for
   --                t: Tree to update
   ----------------------------------------------------------
   procedure Calculate_Std_Dev( qual : in Fruit_Qual; t : in out Tree ) 
   is
      
      use Ada.Numerics.Elementary_Functions;
      sum     : Float := 0.0;

      average : Float   := 0.0;
      std_dev : Float   := 0.0;
   begin
      case qual is
         when Q_SIZE =>
            average := t.stats.s_stat.average;
         when Q_FIRMNESS =>
            average := t.stats.f_stat.average;
         when Q_TASTE =>
            average := t.stats.t_stat.average;
      end case;

      for i in 1..t.f_count loop
         sum := sum + ((Get_Fruit_Value( t.fruits(i), qual) - average) **2);
      end loop;


      std_dev := Sqrt( sum / Float(t.f_count) );

      case qual is
         when Q_SIZE =>
            t.stats.s_stat.std_dev := std_dev;
         when Q_FIRMNESS =>
            t.stats.f_stat.std_dev := std_dev;
         when Q_TASTE =>
            t.stats.t_stat.std_dev := std_dev;
      end case;
   end Calculate_Std_Dev;


   ----------------------------------------------------------
   -- Purpose: Update the average and standard deviation stats for all
   --     qualities based on this Tree's fruit.
   -- Parameters: t: Tree to update
   ----------------------------------------------------------
   procedure Update_Tree_Stats( t : in out Tree ) 
   is
   begin
      -- Do Firmness
      Calculate_Average( Q_Firmness, t );
      Calculate_Std_Dev( Q_FIRMNESS, t );

      -- Do Taste
      Calculate_Average( Q_TASTE, t );
      Calculate_Std_Dev( Q_TASTE, t );

      -- Do Size
      Calculate_Average( Q_SIZE, t );
      Calculate_Std_Dev( Q_SIZE, t );
   end Update_Tree_Stats;


   ----------------------------------------------------------
   -- Purpose: Lookup the numeric value of a fruit's quality
   -- Parameters:    f: which fruit to evaluate
   --              key: which quality to lookup
   -- Returns: The Float value of the Fruit_Key for the given Fruit
   ----------------------------------------------------------
   function Get_Fruit_Value( f : in Fruit; qual : in Fruit_Qual) return Float 
   is
      value : Integer;
   begin
      case qual is
         when Q_SIZE =>
            value := Size'Pos(f.f_size) + 1;
         when Q_FIRMNESS =>
            value :=  Firmness'Pos(f.f_firm) + 1;
         when Q_TASTE =>
            value :=  Taste'Pos(f.f_taste) + 1;
      end case;

      return Float(value);
   end Get_Fruit_Value;
   
   
   ----------------------------------------------------------
   -- Purpose: Print the previous line number
   -- Parameters: input_file: File_Type to look in for a line number
   ----------------------------------------------------------
   procedure Print_Error_Line( input_file : in File_Type)   
   is
      line_num : Integer := Integer( Line( input_file ) )-1;
   begin
      Put( "Error on Line " ); 
      Put( line_num, 0 );   
   end Print_Error_Line;
   
   
   ----------------------------------------------------------
   -- Purpose: Read a Tree in from the input_filem placing or locating
   --    it within the Tree_List.
   -- Parameters: tl: Tree_List to find/put the new Tree into
   --            pos: Position of the Tree within the Tree_List
   --     input_file: File to read the Tree from.
   ----------------------------------------------------------
   procedure Parse_Tree( tl         : in out Tree_List;
                         pos        : out Natural;
                         input_file : in out File_Type)
   is
      c_tree : Tree;
   begin
      Get_Tree( c_tree, input_file );

      -- If tree it not in the list, add it
      if not In_List( tl, pos, c_tree) then
         tl.count      := tl.count + 1;
         pos           := tl.count;
         tl.trees(pos) := c_tree;
      end if;

   exception
      when Tree_Exp =>
         Print_Error_Line( input_file);
         Put_Line(": Invalid Tree ID.");
         pos := 0;
   end Parse_Tree;

   
   ----------------------------------------------------------
   -- Purpose: Read a Fruit from input_file and attach it to c_tree
   -- Parameters: c_tree: Tree to attach the Fruit to
   --         input_file: File to read the Fruit from
   ----------------------------------------------------------
   procedure Parse_Fruit( c_tree : in out Tree;
                          input_file : in out File_Type ) 
   is
      c_fruit : Fruit;
   begin
      Get_Fruit( c_fruit, input_file );

      c_tree.f_count := c_tree.f_count + 1;
      c_tree.fruits( c_tree.f_count ) := c_fruit;

      Update_Tree_Stats(c_tree);

   exception
      when Fruit_Exp =>
         Print_Error_Line( input_file);
         Put(": Bad Fruit quality for Tree "); Put( Integer(c_tree.id), 7 );
         New_Line;
   end Parse_Fruit;
end Fruit_Tree;
