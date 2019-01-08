# RoboSONE

## Installation

##### Installation for [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)

#### setup and run Commands :~

~$ cd RoboSONE

~$ stack setup

~$ stack build

than open 3 diffrent terminals one for UI, second for Grid Interpreter
and third for Robot Interpreter

for UI run Commands below

~$ stack ghci -l src/UI.hs

~$ gridB

for Grid Interpreter run Commands

~$ stack ghci -l src/GridInterpreter.hs

~$ mygame

for Robot Interpreter run Commands

~$ stack ghci -l src/RoboInterpreter.hs

~$ parseFile r1.rbt

## Description

#### UI

the UI is part which gives a view to Grid world which is independent of all functional logic it take
its Grid information from shared file between processes UI and GridInterpreter and request for input
 also by shared file, the code for this is in file

~$ src/UI.hs

#### Grid InterPreter

the Grid Interpreter is part which runs all function logic of gird world where the all robots are and
what actions they are taking and it also handle when robot is killed in Gird World then it is also
responsible to kill that perticuler process and when a new Robot comes in picture it will get to know
by shared file for all new Robots and all Robots also share there next action by shared files which
are created by Robot Interpreter

#### Robot Interpreter

the Robot Interpreter is part which handle all means of Robot program which is executed by parseFile
method in Installation section Robot InterPreter takes a Robot program like r1.rbt and parse it and
then Interpret the code and send the command according to instruction given by program to the Grid
Interpreter than the further part will handle by Grid Interpreter its actually a Robot program
Interpreter and executer, Gird Interpreter and Robot Interpreter also communicate for request
command and response of command by the shared files. and Grid Interpreter works like Operating System
for running Robot Programs

### Source Code [Robot World](https://github.com/HelloSunilSaini/RoboSONE)

#### Thanks
