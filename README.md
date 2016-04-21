# piet-cs345
Implementation of Piet in the form of a Scala DSL for cs 345.

### to-do:
 -  Pre-processing
    * taking in a text file (a grid, with the hex code for each color)
    * codels and color blocks representation (probably as discrete classes)
    * probably a good idea to store the different edges (in a color block) to save time on calculations
-  Processing
    * Two different types of pointers
    * Rules on evaluating a given picture 
    * http://www.dangermouse.net/esoteric/piet.html
    * Separate from evaluating (i.e. process all at first, then evaluate)
-   Evaluation
    * Set up stack
    * Definitions for the commands
    * Take in a list of the evaluated program
