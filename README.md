You must have sbt(scala built tool installed):

cd into directory
sbt
run "<full path to input file>"

There are two parser test inputs. "parsetest.dat" should parse successfully and "parseTermSkipping.dat" will result in errors.

Notes:

Common objects (Grammar Symbols, Errors, ect.) are in package compiler
Each component of the compiler has it's own driver with a method called run.
