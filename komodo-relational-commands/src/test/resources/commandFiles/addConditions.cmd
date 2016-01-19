#
# This play file creates a set of conditions in /myVDB1/myDatarole1/myPermission1/
# 
play ./src/test/resources/commandFiles/addPermissions.cmd
cd myPermission1
add-condition myCondition1
add-condition myCondition2
add-condition MyCondition3