#
# This command file creates a set of user defined functions in myVDB1/myModel1

play ./src/test/resources/commandFiles/addModels.cmd
cd myModel1
add-user-defined-function myUserDefinedFunction1
add-user-defined-function myUserDefinedFunction2
add-user-defined-function MyUserDefinedFunction3