-- Name: Andrew McGuiness
-- Date: October 21, 2017
-- Course: ITEC 320 Procedural Analysis and Design
--
-- package body for the Commands package
--
-- Purpose: The primary purpose of the commands Package is to simplify the main
--    procedure.  The program and be in one of two states, the second state is
--    a command driven interactive state which is handled by the Commands
--    package.

package body Commands is
   procedure Trees_Command(    tl : in Tree_List );
   procedure Fruits_Command(   tl : in Tree_List );
   procedure Averages_Command( tl : in Tree_List );


   ----------------------------------------------------------
   -- Purpose: Print all the Trees in the Tree_List along with
   --    their Fruit count.
   -- Parameters: tl: Tree_List to print
   ----------------------------------------------------------
   procedure Trees_Command( tl : in Tree_List )
   is
   begin
      for t in 1 .. tl.count loop
         Put_Tree( tl.trees(t) );
         New_Line;
      end loop;
   end Trees_Command;


   ----------------------------------------------------------
   -- Purpose: Print all the Fruits of all Trees in the
   --    Tree_List along with their Fruit count.
   -- Parameters: tl: Tree_List to print
   ----------------------------------------------------------
   procedure Fruits_Command( tl : in Tree_List )
   is
   begin
      for t in 1 .. tl.count loop
         Put_Tree( tl.trees(t) );
         Put(":");
         New_Line;
         for f in 1 .. tl.trees(t).f_count loop
            Put_Fruit( tl.trees(t).fruits(f) );
            New_Line;
         end loop;
         Put_Tree_Stats( tl.trees(t).stats );
         New_Line;
      end loop;
   end Fruits_Command;


   ----------------------------------------------------------
   -- Purpose: Print all the Fruit stats of all Trees in the
   --    Tree_List along with their Fruit count.
   -- Parameters: tl: Tree_List to print
   ----------------------------------------------------------
   procedure Averages_Command( tl : in Tree_List )
   is
   begin
      for t in 1 .. tl.count loop
         Put_Tree( tl.trees(t) );
         Put(":");
         New_Line;
         Put_Tree_Stats( tl.trees(t).stats );
         New_Line;
      end loop;
   end Averages_Command;


   ----------------------------------------------------------------------------
   -- Process_Commands --------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Process_Commands( tl : in Tree_List)
   is
      package Command_IO  is new Enumeration_IO(Command);
      use Command_IO;
      c_command : Command;
   begin
      loop
         begin
            Get(c_command);

            case c_command is
               when TREES =>
                  Trees_Command(tl);
               when FRUITS =>
                  Fruits_Command(tl);
               when AVERAGES =>
                  Averages_Command(tl);
               when QUIT =>
                  return;
            end case;

         exception
            when End_Error =>
               Put_Line("End of File Reached.");
               return;

            when Data_Error =>
               Put_Line("Invalid Command.");
               Skip_Line;

            when others =>
               Put_Line("An unknown error has occurred, exiting...");
               return;
         end;
      end loop;
   end Process_Commands;
end Commands;
