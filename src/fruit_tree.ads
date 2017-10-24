-- Name: Andrew McGuiness
-- Date: October 21, 2017
-- Course: ITEC 320 Procedural Analysis and Design
-- 
-- package specification for the Fruit_Tree package
--
-- Purpose: The primary purpose of Fruit_Tree package is to simplify the main 
--    procedure.  The program can be in one of two states, the first state is 
--    an parsing mode where it builds a Tree_List, this state is handled here, 
--    by the Fruit_Tree package.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package Fruit_Tree is
   ID_Exp    : exception;
   Tree_Exp  : exception;
   Fruit_Exp : exception;
      
   MAX_TREES : Constant Positive := 1000;
   MAX_FRUIT : Constant Positive := 20;
   MAX_ID    : Constant Positive := 9999999;

   -- Used to help lookup specific qualities for all Fruit of a Tree
   type Fruit_Qual is (Q_SIZE, Q_FIRMNESS, Q_TASTE);
   
   type Size      is (SMALL, MIDSIZE, LARGE);
   type Firmness  is (SOFT, FIRM, HARD);
   type Taste     is (BLAND, SWEET, SOUR);
   
   
   -- Create an use IO for the Fruit Qualities
   package Size_IO     is new Enumeration_IO(Size);
   package Firmness_IO is new Enumeration_IO(Firmness);
   package Taste_IO    is new Enumeration_IO(Taste);
   use Size_IO;    
   use Firmness_IO;
   use Taste_IO;
   
   -- A Single sample of a Fruit taken from a Tree, represented with three 
   -- Qualities (Size, Firmness and Taste).
   type Fruit is record
      f_size : Size;
      f_firm : Firmness;
      f_taste: Taste;
   end record;
   
   -- An Array of Fruits
   type Fruit_Array is array(Positive range <>) of Fruit;
   
   -- A Logical pair of statistical values
   type Stat_Pair is record
      average : Float := 0.0;
      std_dev : Float := 0.0;
   end record;
   
   -- Collection of State_Pairs that summarize the Qualities of a Tree's Fruits
   type Tree_Stat is record
      f_stat : Stat_Pair;
      t_stat : Stat_Pair;
      s_stat : Stat_Pair;
   end record;
   
   -- A Tree's unique identification number
   type ID_Number is new Natural range 0..MAX_ID;
   
   -- Represents a single Tree
   type Tree is record
      id      : ID_Number;
      fruits  : Fruit_Array(1..MAX_FRUIT);
      f_count : Natural := 0;
      stats   : Tree_Stat;
   end record;
   
   -- An Array of Trees
   type Tree_Array is array(Positive range <>) of Tree;

   -- Collection of Trees
   type Tree_List is record
      trees : Tree_Array(1..MAX_TREES);
      count : Natural := 0;
   end record;
   
   
   ----------------------------------------------------------
   -- Purpose: Compare two the ID's of two trees for equality
   -- Parameters: left, right: Trees to compare
   -- Returns: True if the ID's match, False otherwise.
   ----------------------------------------------------------
   function "=" (left : Tree; right : Tree ) return Boolean;
   
   
   ----------------------------------------------------------
   -- Purpose: Prints a tree to the standard output
   -- Parameters: t: Tree to print
   ----------------------------------------------------------
   procedure Put_Tree( t : in  Tree );
   
   
   ----------------------------------------------------------
   -- Purpose: Reads a Tree from the input_file
   -- Parameters:         t: Tree to read the data into
   --            input_file: File to read the data from
   -- Precondition: input_file must be a valid file
   -- Postcondition: t will contain an ID_Number
   ----------------------------------------------------------
   procedure Get_Tree( t : out Tree; input_file : in out File_Type);
   
   
   ----------------------------------------------------------
   -- Purpose: Prints the average and standard deviation of a Tree_State
   -- Parameters: ts: Tree_Stat to print
   ----------------------------------------------------------
   procedure Put_Tree_Stats( ts : in Tree_Stat);
   
   
   ----------------------------------------------------------
   -- Purpose: Prints a fruit to the standard output
   -- Parameters: f: Fruit to print
   ----------------------------------------------------------
   procedure Put_Fruit( f : in  Fruit );
   
   
   ----------------------------------------------------------
   -- Purpose: Parse a Tree_List out of a file.  The file used
   --   is the furst command line argument.
   -- Parameters: tl: Tree_List to read the input into.
   ----------------------------------------------------------
   procedure Parse_Input_File(tl : in out Tree_List );
   
   
private   
   ----------------------------------------------------------
   -- Purpose: Read a Tree in from the input_filem placing or locating
   --    iti within the Tree_List.
   -- Parameters: tl: Tree_List to find/put the new Tree into
   --            pos: Position of the Tree within the Tree_List
   --     input_file: File to read the Tree from.
   ----------------------------------------------------------
   procedure Parse_Tree( tl : in out Tree_List;
                         pos : out Natural;
                         input_file : in out File_Type);
   
   
   ----------------------------------------------------------
   -- Purpose: Read a Fruit from input_file and attach it to c_tree
   -- Parameters: c_tree: Tree to attach the Fruit to
   --         input_file: File to read the Fruit from
   ----------------------------------------------------------
   procedure Parse_Fruit( c_tree : in out Tree; input_file : in out File_Type );
   
   
   ----------------------------------------------------------
   -- Purpose: Lookup the numeric value of a fruit's quality
   -- Parameters:    f: which fruit to evaluate
   --              key: which quality to lookup
   -- Returns: The Float value of the Fruit_Key for the given Fruit
   ----------------------------------------------------------
   function Get_Fruit_Value( f : in Fruit; qual : in Fruit_Qual) return Float;
   
   
   ----------------------------------------------------------
   -- Purpose: Reads Fruit Qualities from the input_file.
   -- Parameters:         f: Fruit to read the data into
   --            input_file: File to read the data from
   -- Precondition: input_file must be a valid file
   -- Postcondition: f will contain a value for each of it's three
   --    qualities
   ----------------------------------------------------------
   procedure Get_Fruit( f : out Fruit; input_file : in out File_Type);
   
  
   ----------------------------------------------------------
   -- Purpose: Determine if a Tree is already in the Tree_List,
   --    if it is, get it's position.
   -- Parameters:    tl: Tree_List to look in for c_tree
   --               pos: position of the c_tree in tl or 0 if not found
   --            c_tree: Tree to look for
   -- Returns: True if c_tree was found inside the list, False otherwise.
   ----------------------------------------------------------
   function In_List(tl : in  Tree_List;
                   pos : out Positive;
                c_tree : in  Tree ) return Boolean;
   
  
   ----------------------------------------------------------
   -- Purpose: Update the average and standard deviation stats for all
   --     qualities based on this Tree's fruit.
   -- Parameters: t: Tree to update
   ----------------------------------------------------------
   procedure Update_Tree_Stats( t : in out Tree );
   
   
   ----------------------------------------------------------
   -- Purpose: Print a Float according to the project specification.
   -- Parameters: num: Float to print
   ----------------------------------------------------------
   procedure Put_Float( num : in Float );
   
   
   ----------------------------------------------------------
   -- Purpose: Calculate the average for specified qualities based of this 
   --    Tree's fruit.
   -- Parameters: qual: Which Fruit Quality to calculate the average for
   --                t: Tree to update
   ----------------------------------------------------------
   procedure Calculate_Average( qual : in Fruit_Qual; t : in out Tree );
   
   
   ----------------------------------------------------------
   -- Purpose: Calculate the standard deviation for specified qualities based 
   --    of this Tree's fruit.
   -- Parameters: qual: Which Fruit Quality to calculate the average for
   --                t: Tree to update
   ----------------------------------------------------------
   procedure Calculate_Std_Dev( qual : in Fruit_Qual; t : in out Tree );
end Fruit_Tree;