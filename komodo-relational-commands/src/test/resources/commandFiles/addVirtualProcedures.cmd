#
# This command file creates a set of virtual procedures defined functions in myVDB1/myModel1

play ./src/test/resources/commandFiles/addModels.cmd
cd myModel1
add-virtual-procedure myVirtualProcedure1
add-virtual-procedure myVirtualProcedure2
add-virtual-procedure MyVirtualProcedure3