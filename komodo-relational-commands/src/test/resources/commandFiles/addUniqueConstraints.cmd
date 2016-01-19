#
# This command file creates a set of unique constraints in myVDB1/myModel1/myTable1
# 
play ./src/test/resources/commandFiles/addTables.cmd
cd myTable1
add-unique-constraint myUniqueConstraint1
add-unique-constraint myUniqueConstraint2
add-unique-constraint MyUniqueConstraint3