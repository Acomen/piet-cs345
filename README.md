# piet-cs345
Implementation of Piet in the form of a Scala DSL for cs 345.

### to-do:
 -  Pre-processing
    * the way color blocks are being created in the program is a little hacky, probably a good idea to fix this
    * probably a good idea to store the different edges (in a color block) to save time on calculations

-  Processing
    * Two different types of pointers
    * Rules on evaluating a given picture 
    * http://www.dangermouse.net/esoteric/piet.html
    * Evaluating and processing will have to go hand in hand since some commands effect the stack

### done:
 - Pre-processing
    * reading an image and turning it into hex
    * codel representation
    * color block representation
    * printing for color blocks/codels

 - Evaluation
    * ProgramStack
    * Stack commands defined
