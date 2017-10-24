-- Name: Andrew McGuiness
-- Date: October 21, 2017
-- Course: ITEC 320 Procedural Analysis and Design
-- 
-- package specification for the Commands package
--
-- Purpose: The primary purpose of the commands Package is to simplify the main 
--    procedure.  The program and be in one of two states, the second state is 
--    a command driven interactive state which is handled by the Commands 
--    package.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Fruit_Tree; use Fruit_Tree;

package Commands is
   ----------------------------------------------------------
   -- Purpose: Print all the Trees in the Tree_List along with
   --    their Fruit count.
   -- Parameters: tl: Tree_List to print
   ----------------------------------------------------------
   procedure Trees_Command( tl : in Tree_List );
   
   
   ----------------------------------------------------------
   -- Purpose: Print all the Fruits of all Trees in the 
   --    Tree_List along with their Fruit count.
   -- Parameters: tl: Tree_List to print
   ----------------------------------------------------------
   procedure Fruits_Command( tl : in Tree_List );
   
   
   ----------------------------------------------------------
   -- Purpose: Print all the Fruit stats of all Trees in the 
   --    Tree_List along with their Fruit count.
   -- Parameters: tl: Tree_List to print
   ----------------------------------------------------------
   procedure Averages_Command( tl : in Tree_List );
   
   
   ----------------------------------------------------------
   -- Purpose: Compare two the ID's of two trees for equality
   -- Parameters: left, right: Trees to compare
   -- Returns: True if the ID's match, False otherwise.
   ----------------------------------------------------------
   procedure Process_Commands( tl : in Tree_List);
   
   
private
   -- Valid Commands that the User can enter during command mode
   type Command is (TREES, FRUITS, AVERAGES, QUIT);
   package Command_IO  is new Enumeration_IO(Command);
   
end Commands;