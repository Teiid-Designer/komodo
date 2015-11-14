### Connect to a local Teiid Server

This sample shows how to connect to a teiid server instance using the VDB Builder cli.  This example also demonstrates how to view the vdbs deployed to the server after you have successfully connected.

You can use __tab completion__ to see the available commands options, or use __help commandName__ to see command details.


### Requirements

* Install VDB Builder cli - refer to the [Installation Instructions](install-cli.md) for details


### Connect to a server

You can connect to a teiid server within the VDBBuilder cli.  The sample session below shows how to connect to the server and view the deployed VDBs.  Note that once you are connected to a server, several more server commands will become available.  You can discover the available server commands in VDBBuilder by typing 'help' after connecting to the server.

![Connect to Server](img/cli-connect-to-server.png)

---
Here is a summary of the commands used for the above session:

* __`list`__ - shows all children at the cli context.  The workspace initially contains no children.
* __`create-teiid myServer`__ - create a Teiid object named __myServer__
* __`cd myServer`__ - 'navigates' down into __myServer__ 
* __`show-properties`__ - shows the property settings for __myServer__. No properties have been set, but the server will default to 'localhost' and admin port 9999.
* __`workspace`__ - navigate to the workspace
* __`set-server myServer`__ - set the server __myServer__ for this session.
* __`show-status`__ - display the workspace status.  Note the current Teiid Instance is now __myServer__, but the status is [Not Connected]
* __`server-connect`__ - connects to __myServer__. 
* __`show-status`__ - display the status again.  This time, note the Teiid Instance status is [Connected]
* __`server-vdbs`__ - display the vdbs currently deployed on the connected server.
* __`server-disconnect`__ - disconnects from the __myServer__.

---
