-- Name: Andrew McGuiness
-- Date: October 26, 2017
-- Course: ITEC 320 Procedural Analysis and Design
--
-- Specification for the Commands package
--
-- Purpose: The Commands Package provides a simplified command listening state
--    to view the data contained inside of a Tree_List.  While in this listen
--    state the Commands package will give the user read-only access to the
--    Tree_List data.

with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Fruit_Tree;     use Fruit_Tree;

package Commands is

-- Public Subroutines ---------------------------------------------------------

   ----------------------------------------------------------
   -- Purpose: Wait for user input, parse that input and execute
   --    a corresponding command.
   -- Parameters: tl: Tree_List to execute commands on
   ----------------------------------------------------------
   procedure Process_Commands (tl : in Tree_List);

private
   -- Valid Commands that the User can enter during "command mode"
   type Command is (TREES, FRUITS, AVERAGES, QUIT);

-- Private Subroutines --------------------------------------------------------

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
