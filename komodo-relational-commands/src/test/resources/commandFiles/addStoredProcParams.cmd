#
# This command file creates a set of parameters in myVDB1/myModel1/myStoredProcedure1

play ./src/test/resources/commandFiles/addStoredProcedures.cmd
cd myStoredProcedure1
add-parameter myParameter1
add-parameter myParameter2
add-parameter MyParameter3