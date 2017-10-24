-- Name: Andrew McGuiness
-- Date: October 21, 2017
-- Course: ITEC 320 Procedural Analysis and Design
--
-- Extra Credit Attempted:
--    Extensive error checking
--
-- Purpose: This program loads a specified file from disk, parses the contents
-- of the file, creating a Tree_List data structure.  After the file has been
-- processed, it is closed and control is handed to the user.  The user can
-- enter any of four commands:
--
--  "Trees": Output only Tree id number and number of fruit samples
--  "Averages": Output as in Trees, plus the average and standard deviation of
--    each category for each tree.
--  "Fruits": Output as in Trees, followed by each fruit and then followed by
--    average and standard deviation, as in Averages
--  "Quit": Halt the program
--
-- Sample content of "trees.dat":
--  tree 1111111
--  fruit small soft bland
--  tree 2222222
--  fruit midsize firm sweet
--  tree 3333333
--  fruit large hard sour
--
-- Program Ran with: dotrees trees.dat
--
-- Sample input:
--  trees
--  fruits
--  averages
--  quit
--
-- Corresponding output:
--  TREE: 1111111   1
--  TREE: 2222222   1
--  TREE: 3333333   1
--  TREE: 1111111   1:
--      SOFT      BLAND     SMALL
--      1.0       1.0       1.0
--      0.0       0.0       0.0
--  TREE: 2222222   1:
--      FIRM      SWEET     MIDSIZE
--      2.0       2.0       2.0
--      0.0       0.0       0.0
--  TREE: 3333333   1:
--      HARD      SOUR      LARGE
--      3.0       3.0       3.0
--      0.0       0.0       0.0
--  TREE: 1111111   1:
--      1.0       1.0       1.0
--      0.0       0.0       0.0
--  TREE: 2222222   1:
--      2.0       2.0       2.0
--      0.0       0.0       0.0
--  TREE: 3333333   1:
--      3.0       3.0       3.0
--      0.0       0.0       0.0

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;

with Fruit_Tree;   use Fruit_Tree;
with Commands; use Commands;

procedure Dotrees is
   ----------------------------------------------------------
   -- Purpose: Validate the command line arguments and ensure the
   --   provided file exists.
   -- Returns: True if a file was specified and it exists, False
   --    otherwise.
   ----------------------------------------------------------
   function Valid_Input_File return Boolean is
      valid : Boolean := True;
   begin
      if Argument_Count = 0 then
         Put_Line("You must specify an input file as a command line argument.");
         valid := False;

      elsif not Exists(Argument(1)) then
         Put_Line("File <" & Argument(1) & "> does not exist.");
         valid := False;
      end if;

      return valid;
   end Valid_Input_File;



   ----------------------------------------------------------------------------
   -- Entry Point -------------------------------------------------------------
   ----------------------------------------------------------------------------
   tl : Tree_List;
begin
   if Valid_Input_File then
      Parse_Input_File( tl );
      New_Line;
      Process_Commands( tl );
   end if;
end Dotrees;
