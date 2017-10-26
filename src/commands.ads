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

with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Fruit_Tree;     use Fruit_Tree;

package Commands is

   -- Public Subroutines ----------------------------------------------------

   ----------------------------------------------------------
   -- Purpose: Wait for user input, parse that input and execute
   --    the corresponding commands.
   -- Parameters: tl: Tree_List to execute commands on
   ----------------------------------------------------------
   procedure Process_Commands (tl : in Tree_List);

private
   -- Valid Commands that the User can enter during command mode
   type Command is (TREES, FRUITS, AVERAGES, QUIT);

   -- Private Subroutines ---------------------------------------------------

   ----------------------------------------------------------
   -- Purpose: Print each Tree ID, followed by Fruit Count.
   -- Parameters: tl: Tree_List to pull data from
   ----------------------------------------------------------
   procedure Trees_Command (tl : in Tree_List);

   ----------------------------------------------------------
   -- Purpose: Print each Tree ID, followed by Fruit Count followed
   --    all Fruit followed by the statistics for the Tree's Fruit.
   -- Parameters: tl: Tree_List to pull data from
   ----------------------------------------------------------
   procedure Fruits_Command (tl : in Tree_List);

   ----------------------------------------------------------
   -- Purpose: Print each Tree ID, followed by Fruit Count followed
   --    by the statistics for the Tree's Fruit.
   -- Parameters: tl: Tree_List to pull data from
   ----------------------------------------------------------
   procedure Averages_Command (tl : in Tree_List);

end Commands;
