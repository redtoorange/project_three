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

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with Ada.Exceptions;      use Ada.Exceptions;
with Ada.Command_Line;    use Ada.Command_Line;
with Ada.Numerics.Elementary_Functions; 

package Fruit_Tree is
   MAX_TREES : Constant Positive := 1000;
   MAX_FRUIT : Constant Positive := 20;
   MAX_ID    : Constant Positive;


   type Tree_Stat   is private;
   type ID_Number   is private;
   type Fruit       is private;
   type Fruit_Array is array(Positive range <>) of Fruit;
   
   
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
   -- Purpose: Parse a Tree_List out of a file.  The file used
   --   is the furst command line argument.
   -- Parameters: tl: Tree_List to read the input into.
   ----------------------------------------------------------
   procedure Parse_Fruit_File(tl : in out Tree_List );
   
   
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
   -- Purpose: Compare two the ID's of two trees for equality
   -- Parameters: left, right: Trees to compare
   -- Returns: True if the ID's match, False otherwise.
   ----------------------------------------------------------
   function "=" (left : Tree; right : Tree ) return Boolean;
   
   
private
   ID_Exp    : exception;
   Tree_Exp  : exception;
   Fruit_Exp : exception;

   
   MAX_ID    : Constant Positive := 9999999;
   
   
   -- Used to help lookup specific qualities for all Fruit of a Tree
   type Fruit_Qual is (Q_SIZE, Q_FIRMNESS, Q_TASTE);
   type Size       is (SMALL, MIDSIZE, LARGE);
   type Firmness   is (SOFT, FIRM, HARD);
   type Taste      is (BLAND, SWEET, SOUR);
   
   
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
      f_size  : Size;
      f_firm  : Firmness;
      f_taste : Taste;
   end record;

   
   -- A Logical pair of statistical values
   type Stat_Pair is record
      average : Float := 0.0;
      std_dev : Float := 0.0;
   end record;
   
   
   -- Collection of State_Pairs that summarize the Qualities of a Tree's Fruits
   type Tree_Stat is record
      f_stat  : Stat_Pair;
      t_stat  : Stat_Pair;
      s_stat  : Stat_Pair;
   end record;
   
   
   -- A Tree's unique identification number
   type ID_Number is new Natural range 0..MAX_ID;
   
   
end Fruit_Tree;
