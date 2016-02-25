#
# This command file creates a set of custom options in myVDB1/myModel1/myTable1
# 
play ./src/test/resources/commandFiles/addTables.cmd
cd myTable1
set-custom-option myCustomOption1 myValue1
set-custom-option myCustomOption2 myValue2
set-custom-option MyCustomOption3 myValue3
set-custom-option my.custom.option myValue1
set-custom-option My"Custom"Option3 myValue3