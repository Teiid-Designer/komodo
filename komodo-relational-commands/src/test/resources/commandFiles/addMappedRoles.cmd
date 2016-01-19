#
# This command file creates a set of MappedRoles in myVDB1/myDatarole1/
# 
play ./src/test/resources/commandFiles/addDataRoles.cmd
cd myDatarole1
add-mapped-role myMappedRole1
add-mapped-role myMappedRole2
add-mapped-role MyMappedRole3
