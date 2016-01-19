#
# This command file creates a set of permissions in myVDB1/myDatarole/

play ./src/test/resources/commandFiles/addDataRoles.cmd
cd myDatarole1
add-permission myPermission1
add-permission myPermission2
add-permission MyPermission3