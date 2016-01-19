#
# This command file creates a set of stored procedures in myVDB1/myModel1

play ./src/test/resources/commandFiles/addModels.cmd
cd myModel1
add-stored-procedure myStoredProcedure1
add-stored-procedure myStoredProcedure2
add-stored-procedure MyStoredProcedure3