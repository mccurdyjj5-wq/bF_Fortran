# bF_Fortran
bF_Fortran is a command line Brainfuck interpreter and compiler suite written in Fortran. It currently only supports Windows, for I have a lack of computers I can install Linux on.
# Usage
bFortran is the interpreter. To use it, just run it with the filename of the program you wish to run, like so:
```
bFortran <input.b>
```
bF2Fortran is the compiler. It converts a given Brainfuck program into an optimized Fortran program. To use it, run it with the input filename and the intended output filename, like so: 
```
bF2Fortran <input.b> [output.f90]
```
The default output filename is 'output.f90'. It will replace the contents of the output destination if a file exists there already, so be careful!
# Compatibility
There are some compatibility issues, as all such Brainfuck interpreters must face. Here are the ones I thought were more important to mention, but there are likely others out there that I do not know of.
## EOF
The most major compatibility issue is EOF, as always. It is currently set-up to return 0x00 on EOF. I am currently working on fixing this, but I am still learning Fortran, so this is currently not a high priority for me, anyways.
## Tape Size
The program currently supports a tape size of 30,000 cells. I do not plan on changing this, but I may add an option to use a custom tape size. I do not know the behavior of going out of bounds on the tape array, but just treat it as like C, please. It definitely doesn't support wrapping around, either way.
## Other Issues
There are some Brainfuck programs that do not work, but I can't figure out why. If anyone who is more knowledgeable than I am would like to help, please do! I can only do so much with my current knowledge and experience.
# Todo
Here is a list of things I would like to implement in the future. This will be updated as time goes on.
- Add --debug command-line option
  - This would dump the tape's contents whenever a # is detected.
- Add --eof [none,0,255] command-line option to change the EOF behavior. Default = 0
- Add --unoptimized flag to bF2Fortran to make the compiler **not** optimize the code. 
