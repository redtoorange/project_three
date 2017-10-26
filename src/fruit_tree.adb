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
-- Public Subroutines ---------------------------------------------------------

   -- Parse_Input_File ------------------------------------------------------
   procedure Parse_Fruit_File (tl : in out Tree_List; success : out Boolean) is
      use Ada.Command_Line;
      KeyWord_Exception : exception;

      type KeyWord is (TREE, FRUIT);
      package KeyWord_IO is new Enumeration_IO (KeyWord);

      next_key   : KeyWord;
      last_index : Natural := 0;
      input_file : File_Type;
   begin
      -- Open our Fruit file and read through it until the end.
      Open (File => input_file, Mode => In_File, Name => Argument (1));

      while not End_Of_File (input_file) loop

         -- Inner Exception Handling to enable recovery from bad input
         begin

            -- Try to use the next token as a KeyWord(Tree or Fruit)
            KeyWord_IO.Get (File => input_file, Item => next_key);

            if next_key = TREE then
               -- Found a Tree, locate it in the list or make a new one.
               Parse_Tree (tl, last_index, input_file);

            elsif next_key = FRUIT then
               -- Ensure that the Fruit has a parent Tree
               if last_index /= 0 then
                  -- Parse the Fruit and attach it to the current Tree.
                  Parse_Fruit (tl.trees (last_index), input_file);
               else
                  -- No Tree to attach to, toss an error out.
                  raise Fruit_Exp;
               end if;
            end if;

         exception
            when Fruit_Exp =>
               -- Unparented Fruit
               Print_Error_Line (input_file);
               Put_Line
                 (": Cannot add fruit, it doesn't have a valid parent.");

            when Data_Error =>
               -- Garbage data found
               Print_Error_Line (input_file);
               Put_Line (": Read a non-keyword token, discarding.");

            when E : others =>
               -- Something else, bad times.
               Put ("Unhandled error: ");
               Put_Line (Exception_Name (E));

               success := False;
               return;
         end;

      end loop;

      success := True;
      Close (File => input_file);
   end Parse_Fruit_File;

   -- Equals Override -------------------------------------------------------
   function "=" (left : Tree; right : Tree) return Boolean is
      equal : Boolean := False;
   begin
      if left.id = right.id then
         equal := True;
      end if;

      return equal;
   end "=";

   -- Put_Tree --------------------------------------------------------------
   procedure Put_Tree (t : in Tree) is
   begin
      Put ("TREE:");
      Set_Col (7);
      Put (Integer (t.id), 7);
      Set_Col (17);
      Put (t.f_count, 0);
   end Put_Tree;

   -- Get_Tree --------------------------------------------------------------
   procedure Get_Tree (t : out Tree; input_file : in out File_Type) is
      input    : Natural;
      next_c   : Character;
      end_line : Boolean;

   begin
      -- Grab the next token in the File.
      Get (input_file, input);

      -- Check the token immediately after the input
      Look_Ahead (input_file, next_c, end_line);

   -- If the next Character was a space or a New_Line, the ID should be valid
      if next_c = ' ' or end_line then
         t.id := ID_Number (input);
      else
         -- The next character was something else, so the ID is invalid
         raise Tree_Exp;
      end if;
   exception
      when others =>
         -- Convert all Exceptions into a Tree Exception to handle them in a
         --   more generic way.
         raise Tree_Exp;
   end Get_Tree;

   -- Put_Fruit -------------------------------------------------------------
   procedure Put_Fruit (f : in Fruit) is
   begin
      Set_Col (5);
      Put (f.f_firm);

      Set_Col (15);
      Put (f.f_taste);

      Set_Col (25);
      Put (f.f_size);
   end Put_Fruit;

   -- Get_Fruit -------------------------------------------------------------
   procedure Get_Fruit (f : out Fruit; input_file : in out File_Type) is
      temp : Fruit;
   begin
      -- Get the three qualities and store them in temp
      Get (input_file, temp.f_size);
      Get (input_file, temp.f_firm);
      Get (input_file, temp.f_taste);

      -- All three qualities were read without errors, set the output
      f := temp;
   exception
      when others =>
         -- One of the qualities was malformed/missing
         raise Fruit_Exp;
   end Get_Fruit;

   -- Put_Tree_Stats --------------------------------------------------------
   procedure Put_Tree_Stats (ts : in Tree_Stat) is
   begin
      Set_Col (5);
      Put_Float (ts.f_stat.average);

      Set_Col (15);
      Put_Float (ts.t_stat.average);

      Set_Col (25);
      Put_Float (ts.s_stat.average);

      New_Line;

      Set_Col (5);
      Put_Float (ts.f_stat.std_dev);

      Set_Col (15);
      Put_Float (ts.t_stat.std_dev);

      Set_Col (25);
      Put_Float (ts.s_stat.std_dev);
   end Put_Tree_Stats;

-- Private Subroutines --------------------------------------------------------

   -- Put_Float -------------------------------------------------------------
   procedure Put_Float (num : in Float) is
      use Ada.Float_Text_IO;
   begin
      Put (num, Fore => 0, Aft => 1, Exp => 0);
   end Put_Float;

   -- In_List ---------------------------------------------------------------
   function In_List
     (tl     : in     Tree_List;
      pos    :    out Positive;
      c_tree : in     Fruit_Tree.Tree) return Boolean
   is
      found : Boolean := False;
   begin
      -- Linear search for the corrent Tree
      for i in 1 .. tl.t_count loop

         -- Found it, set the pos
         if c_tree = tl.trees (i) then
            pos   := i;
            found := True;
         end if;

         exit when found;
      end loop;

      return found;
   end In_List;

   -- Calculate_Average -----------------------------------------------------
   procedure Calculate_Average (qual : in Fruit_Qual; t : in out Tree) is
      sum     : Float := 0.0;
      average : Float := 0.0;
   begin
      -- Sum the value of the passed quality for the Tree
      for i in 1 .. t.f_count loop
         sum := sum + Get_Fruit_Value (t.fruits (i), qual);
      end loop;

      average := sum / Float (t.f_count);

      -- Set the stat based on the quality we were checking
      case qual is
         when Q_SIZE =>
            t.f_stats.s_stat.average := average;
         when Q_FIRMNESS =>
            t.f_stats.f_stat.average := average;
         when Q_TASTE =>
            t.f_stats.t_stat.average := average;
      end case;
   end Calculate_Average;

   -- Calculate_Std_Dev -----------------------------------------------------
   procedure Calculate_Std_Dev (qual : in Fruit_Qual; t : in out Tree) is

      use Ada.Numerics.Elementary_Functions;
      sum : Float := 0.0;

      average : Float := 0.0;
      std_dev : Float := 0.0;
   begin
      -- Grab the average stat from the Tree
      case qual is
         when Q_SIZE =>
            average := t.f_stats.s_stat.average;
         when Q_FIRMNESS =>
            average := t.f_stats.f_stat.average;
         when Q_TASTE =>
            average := t.f_stats.t_stat.average;
      end case;

      -- Perform the summation based on the Standard Deviation forumla
      for i in 1 .. t.f_count loop
         sum := sum + ((Get_Fruit_Value (t.fruits (i), qual) - average)**2);
      end loop;

      std_dev := Sqrt (sum / Float (t.f_count));

      -- Set the corresponding quality
      case qual is
         when Q_SIZE =>
            t.f_stats.s_stat.std_dev := std_dev;
         when Q_FIRMNESS =>
            t.f_stats.f_stat.std_dev := std_dev;
         when Q_TASTE =>
            t.f_stats.t_stat.std_dev := std_dev;
      end case;
   end Calculate_Std_Dev;

   -- Update_Tree_Stats -----------------------------------------------------
   procedure Update_Tree_Stats (t : in out Tree) is
   begin
      -- Update Firmness
      Calculate_Average (Q_FIRMNESS, t);
      Calculate_Std_Dev (Q_FIRMNESS, t);

      -- Update Taste
      Calculate_Average (Q_TASTE, t);
      Calculate_Std_Dev (Q_TASTE, t);

      -- Update Size
      Calculate_Average (Q_SIZE, t);
      Calculate_Std_Dev (Q_SIZE, t);
   end Update_Tree_Stats;

   -- Get_Fruit_Value -------------------------------------------------------
   function Get_Fruit_Value
     (f    : in Fruit;
      qual : in Fruit_Qual) return Float
   is
      value : Integer;
   begin
      -- Convert the quality into a numerical value
      case qual is
         when Q_SIZE =>
            value := Size'Pos (f.f_size) + 1;
         when Q_FIRMNESS =>
            value := Firmness'Pos (f.f_firm) + 1;
         when Q_TASTE =>
            value := Taste'Pos (f.f_taste) + 1;
      end case;

      return Float (value);
   end Get_Fruit_Value;

   -- Print_Error_Line ------------------------------------------------------
   procedure Print_Error_Line (input_file : in File_Type) is
      line_num : Integer := Integer (Line (input_file));
   begin
      Put ("Error on Line ");
      Put (line_num, 0);
   end Print_Error_Line;

   -- Parse_Tree ------------------------------------------------------------
   procedure Parse_Tree
     (tl         : in out Tree_List;
      pos        :    out Natural;
      input_file : in out File_Type)
   is
      c_tree : Tree;
   begin
      -- Read a Tree from the file
      Get_Tree (c_tree, input_file);

      -- If tree it not in the list, add it
      if not In_List (tl, pos, c_tree) then
         tl.t_count     := tl.t_count + 1;
         pos            := tl.t_count;
         tl.trees (pos) := c_tree;
      end if;

   exception
      when Tree_Exp =>
         -- Something was bad with the Tree ID, let the user know
         Print_Error_Line (input_file);
         Put_Line (": Invalid Tree ID.");
         pos := 0;
   end Parse_Tree;

   -- Parse_Fruit -----------------------------------------------------------
   procedure Parse_Fruit
     (c_tree     : in out Tree;
      input_file : in out File_Type)
   is
      c_fruit : Fruit;
   begin
      -- Read in Fruit qualities from the file
      Get_Fruit (c_fruit, input_file);

      -- Attach the Fruit to the Tree
      c_tree.f_count                 := c_tree.f_count + 1;
      c_tree.fruits (c_tree.f_count) := c_fruit;

      -- Update the Average and Standard Deviation for the Tree
      Update_Tree_Stats (c_tree);

   exception
      when Fruit_Exp =>
         -- One of the qualities was missing/malformed, don't add the Fruit
         Print_Error_Line (input_file);
         Put (": Bad Fruit quality for Tree ");
         Put (Integer (c_tree.id), 7);
         New_Line;
   end Parse_Fruit;
end Fruit_Tree;
