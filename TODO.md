# Fixes
* Fill out error handling
* Larger stack
* More locals
* Change object .toFOO functions to be generic
* Use slices for native function arguments
* Define arity of native functions (superseeds the above?)
* Use union for objects
 
# Optimisation
* Add a two byte instruction for loading a number 0..255
  * As above but for -255..0
* Remove double hash of new strings in the string interner
* Look for variable names in constant table rather than adding them multiple times
* Store ip locally
* Only use closures where needed

# Language features
* Arrays
* Hashmaps
  * Will require making the hashmap generic on its key
* Constants
* Continue/break in loops

# Testing
* Assert the state of the global variables
* Assert the state of the output (ie prints)
