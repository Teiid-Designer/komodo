/*************************************************************************************
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership. Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 ************************************************************************************/
package org.komodo.shell.commands.core;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.InvalidCommandArgumentException;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.ContextUtils;
import org.komodo.shell.util.PrintUtils;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidPropertyDefinition;
import org.komodo.spi.runtime.TeiidTranslator;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.utils.StringUtils;

/**
 * Server Command.  Server-related commands
 * server < connect | disconnect | show | deploy | undeploy | copyvdb >
 *
 */
public class ServerCommand extends BuiltInShellCommand implements StringConstants {

    /**
     * The command name.
     */
    public static final String NAME = "server"; //$NON-NLS-1$

    private static final String SUBCMD_SET = "set"; //$NON-NLS-1$
    private static final String SUBCMD_CONNECT = "connect"; //$NON-NLS-1$
    private static final String SUBCMD_DISCONNECT = "disconnect"; //$NON-NLS-1$
    private static final String SUBCMD_SHOW = "show"; //$NON-NLS-1$
    private static final String SUBCMD_DEPLOY = "deploy"; //$NON-NLS-1$
    private static final String SUBCMD_UNDEPLOY = "undeploy"; //$NON-NLS-1$
    private static final String SUBCMD_IMPORTVDB = "importvdb"; //$NON-NLS-1$
    private static final List<String> SUBCMDS = Arrays.asList(SUBCMD_SET,SUBCMD_CONNECT, SUBCMD_DISCONNECT, SUBCMD_SHOW, SUBCMD_DEPLOY, SUBCMD_UNDEPLOY, SUBCMD_IMPORTVDB);
    
    private static final String VDBS = "vdbs"; //$NON-NLS-1$
    private static final String TRANSLATORS = "translators"; //$NON-NLS-1$
    private static final String DATASOURCES = "datasources"; //$NON-NLS-1$
    private static final String DATASOURCE_TYPES = "datasource_types"; //$NON-NLS-1$
    private static final String VDB = "vdb"; //$NON-NLS-1$
    private static final String TRANSLATOR = "translator"; //$NON-NLS-1$
    private static final String DATASOURCE = "datasource"; //$NON-NLS-1$
    private static final String DATASOURCE_TYPE = "datasource_type"; //$NON-NLS-1$
    
    private static final List< String > SERVER_OBJ_TYPES_PLURAL = Arrays.asList( VDBS, TRANSLATORS, DATASOURCES, DATASOURCE_TYPES);
    private static final List< String > SERVER_OBJ_TYPES_SINGULAR = Arrays.asList( VDB, TRANSLATOR, DATASOURCE, DATASOURCE_TYPE);
    private static final List< String > SERVER_OBJ_TYPES_ALL = Arrays.asList(VDBS, VDB, TRANSLATORS, TRANSLATOR, DATASOURCES, DATASOURCE, DATASOURCE_TYPES, DATASOURCE_TYPE);

    private static final List< String > SERVER_OBJ_DEPLOY_TYPES = Arrays.asList( VDB, DATASOURCE );
    
    private static final String VDB_DEPLOYMENT_SUFFIX = "-vdb.xml"; //$NON-NLS-1$
    private static final String TEMPFILE_PREFIX = "Vdb-"; //$NON-NLS-1$
    private static final String TEMPFILE_SUFFIX = ".xml"; //$NON-NLS-1$
    
    /**
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     */
    public ServerCommand( final WorkspaceStatus wsStatus ) {
        super( wsStatus, NAME );
    }

	/**
	 * @see org.komodo.shell.api.ShellCommand#execute()
	 */
	@Override
    public boolean execute() throws Exception {
		// Make sure the correct number of args supplied
        String subcmdArg = requiredArgument(0, Messages.getString(Messages.ServerCommand.InvalidArgMsg_SubCommand));
        
        WorkspaceStatus wsStatus = getWorkspaceStatus();
        Teiid teiid = wsStatus.getTeiid();

        // All subcommands except 'setdefault' require a teiid definition
        if(!SUBCMD_SET.equalsIgnoreCase(subcmdArg) && teiid == null) {
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ServerCommand.NoTeiidDefined));
            return false;
        }

        try {
            // Sub-command which sets the default server
            if (SUBCMD_SET.equalsIgnoreCase(subcmdArg)) {
                // teiid name is required
                String nameOrId = requiredArgument(1, Messages.getString(Messages.ServerCommand.InvalidArgMsg_ServerName));

                WorkspaceStatus wStatus = getWorkspaceStatus();

                WorkspaceManager wkspManager = wStatus.getCurrentContext().getWorkspaceManager();

                List<Teiid> teiids = wkspManager.findTeiids(wStatus.getTransaction());

                if (teiids == null || teiids.size() == 0) {
                    print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ServerCommand.noTeiidInstancesDefined));
                    return false;
                }

                boolean teiidFound = false;
                for (Teiid theTeiid : teiids) {
                    String teiidName = theTeiid.getName(wStatus.getTransaction());
                    if (nameOrId.equals(theTeiid.getId(wStatus.getTransaction())) || nameOrId.equals(teiidName)) {
                        // Set current teiid
                        wStatus.setTeiid(theTeiid);
                        teiidFound = true;
                        print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ServerCommand.teiidSetOk, teiidName));
                    }
                }

                if(!teiidFound) {
                    print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ServerCommand.noTeiidWithName, nameOrId));
                    return false;
                }
            // Sub-command which connects to the default server
            } else if (SUBCMD_CONNECT.equalsIgnoreCase(subcmdArg)) {
                
                print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ServerCommand.AttemptingToConnect,teiid.getName(wsStatus.getTransaction())));
                TeiidInstance teiidInstance = teiid.getTeiidInstance(wsStatus.getTransaction());
                try {
                    teiidInstance.connect();
                } catch (Exception ex) {
                    print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ServerCommand.ConnectionError,ex.getLocalizedMessage()));
                }
                
                boolean connected = teiidInstance.isConnected();
                String connectStatus = connected ? Messages.getString(Messages.ServerCommand.Connected) : Messages.getString(Messages.ServerCommand.NotConnected); 
                print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ServerCommand.TeiidStatus, teiid.getName(wsStatus.getTransaction()), connectStatus)); 
                return connected;
            // Sub-command which disconnects from the default server
        	} else if (SUBCMD_DISCONNECT.equalsIgnoreCase(subcmdArg)) {
                
                print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ServerCommand.AttemptingToDisconnect,teiid.getName(wsStatus.getTransaction())));
                TeiidInstance teiidInstance = teiid.getTeiidInstance(wsStatus.getTransaction());
                if(!teiidInstance.isConnected()) {
                    print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ServerCommand.NoServerToDisconnectMsg));
                }
                
                teiidInstance.disconnect();
                print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ServerCommand.DisconnectSuccessMsg, teiid.getName(wsStatus.getTransaction())));
                return true;
            // Sub-command for showing objects and object details for the connected server
            } else if (SUBCMD_SHOW.equalsIgnoreCase(subcmdArg)) {
                String serverObjType = requiredArgument(1, Messages.getString(Messages.ServerCommand.InvalidArgMsg_ServerObjType));

                if(!isTeiidConnected(wsStatus)) {
                    print( MESSAGE_INDENT, Messages.getString( Messages.ServerCommand.ServerNotConnected ) ); 
                    return false;
                }
                
                if(!SERVER_OBJ_TYPES_PLURAL.contains(serverObjType.toLowerCase()) && !SERVER_OBJ_TYPES_SINGULAR.contains(serverObjType.toLowerCase())) {
                    print( MESSAGE_INDENT, Messages.getString( Messages.ServerCommand.InvalidServerObjectType, serverObjType ) ); 
                    return false;
                }
                
                // Plural type - shows names of all
                if(SERVER_OBJ_TYPES_PLURAL.contains(serverObjType.toLowerCase())) {
                    printServerObjectNames(wsStatus,serverObjType);
                }
                
                // Singular type - requires name of the object, then shows object properties
                if(SERVER_OBJ_TYPES_SINGULAR.contains(serverObjType.toLowerCase())) {
                    String serverObjName = requiredArgument(2, Messages.getString(Messages.ServerCommand.InvalidArgMsg_ServerObjName));
                    printServerObjectDetails(wsStatus,serverObjType,serverObjName);
                }
            // Sub-command for doing deployments to the connected server
            } else if (SUBCMD_DEPLOY.equalsIgnoreCase(subcmdArg)) {
                String deployObjType = requiredArgument(1, Messages.getString(Messages.ServerCommand.InvalidArgMsg_DeployServerObjType));
                String deployObjPath = requiredArgument(2, Messages.getString(Messages.ServerCommand.InvalidArgMsg_DeployServerObjName));
                
                if(!isTeiidConnected(wsStatus)) {
                    print( MESSAGE_INDENT, Messages.getString( Messages.ServerCommand.ServerNotConnected ) ); 
                    return false;
                }
                
                if(!SERVER_OBJ_DEPLOY_TYPES.contains(deployObjType.toLowerCase())) {
                    print( MESSAGE_INDENT, Messages.getString( Messages.ServerCommand.InvalidServerDeployObjectType, deployObjType ) ); 
                    return false;
                }
                
                TeiidInstance teiidInstance = teiid.getTeiidInstance(wsStatus.getTransaction());
                
                // VDB deployment
                if(VDB.equalsIgnoreCase(deployObjType.toLowerCase())) {
                    WorkspaceContext context = ContextUtils.getContextForPath(wsStatus, deployObjPath);
                    KomodoObject vdbToDeploy = context.getKomodoObj();
                    
                    WorkspaceManager wkspManager = wsStatus.getCurrentContext().getWorkspaceManager();

                    Vdb vdb = wkspManager.resolve(wsStatus.getTransaction(), vdbToDeploy, Vdb.class);
                    if (vdb == null)
                        throw new InvalidCommandArgumentException(0, Messages.getString(Messages.ServerCommand.CouldNotResolve));

                    String vdbXml = vdb.export(wsStatus.getTransaction(), null);
                    if (vdbXml == null || vdbXml.isEmpty()) {
                        print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ServerCommand.VdbExportFailed));
                        return false;
                    }

                    String vdbName = vdb.getName(wsStatus.getTransaction());
                    String vdbDeploymentName = vdbName + VDB_DEPLOYMENT_SUFFIX;
                    InputStream stream = new ByteArrayInputStream(vdbXml.getBytes());
                    teiidInstance.deployDynamicVdb(vdbDeploymentName, stream);
                    
                    print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ServerCommand.VdbDeployFinished));
                }
            // Sub-command for doing undeployments on the connected server
            } else if (SUBCMD_UNDEPLOY.equalsIgnoreCase(subcmdArg)) {
                String deployedObjType = requiredArgument(1, Messages.getString(Messages.ServerCommand.InvalidArgMsg_DeployServerObjType));
                String deployedObjName = requiredArgument(2, Messages.getString(Messages.ServerCommand.InvalidArgMsg_ServerObjName));
                
                if(!isTeiidConnected(wsStatus)) {
                    print( MESSAGE_INDENT, Messages.getString( Messages.ServerCommand.ServerNotConnected ) ); 
                    return false;
                }
                
                if(!SERVER_OBJ_DEPLOY_TYPES.contains(deployedObjType.toLowerCase())) {
                    print( MESSAGE_INDENT, Messages.getString( Messages.ServerCommand.InvalidServerDeployObjectType, deployedObjType ) ); 
                    return false;
                }
                
                TeiidInstance teiidInstance = teiid.getTeiidInstance(wsStatus.getTransaction());
                
                // VDB deployment
                if(VDB.equalsIgnoreCase(deployedObjType.toLowerCase())) {
                    TeiidVdb vdb = teiidInstance.getVdb(deployedObjName);
                    if(vdb!=null) {
                        if(vdb.isXmlDeployment()) {
                            teiidInstance.undeployDynamicVdb(vdb.getName());
                        } else {
                            teiidInstance.undeployVdb(vdb.getName());
                        }
                    }
                    
                    print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ServerCommand.VdbUnDeployFinished));
                }
            // Sub-command which imports a VDB from the connected server
            } else if (SUBCMD_IMPORTVDB.equalsIgnoreCase(subcmdArg)) {
                String vdbName = requiredArgument(1, Messages.getString(Messages.ServerCommand.InvalidArgMsg_ServerObjName));
                
                if(!isTeiidConnected(wsStatus)) {
                    print( MESSAGE_INDENT, Messages.getString( Messages.ServerCommand.ServerNotConnected ) ); 
                    return false;
                }
                
                TeiidInstance teiidInstance = teiid.getTeiidInstance(wsStatus.getTransaction());
                
                // Get the VDB - make sure its a dynamic VDB
                TeiidVdb vdb = teiidInstance.getVdb(vdbName);
                if(vdb == null) {
                    print( MESSAGE_INDENT, Messages.getString( Messages.ServerCommand.ServerItemNotFound, vdbName ) ); 
                    return false;
                }
                if(!vdb.isXmlDeployment()) {
                    print( MESSAGE_INDENT, Messages.getString( Messages.ServerCommand.CanOnlyCopyDynamicVDBs, vdbName ) ); 
                    return false;
                }
                
                // Export the string content
                String vdbStr = vdb.export();
                
                // Output the content to a temp file
                File tempFile = File.createTempFile(TEMPFILE_PREFIX, TEMPFILE_SUFFIX);
                writeToFile(tempFile.getPath(),vdbStr);
                
                // Run the importer using the temp file
                ImportCommand importCommand = new ImportCommand(getWorkspaceStatus());
                importCommand.setArguments(new Arguments( "VDB" + StringConstants.SPACE + tempFile.getPath() )); //$NON-NLS-1$
                importCommand.execute();
                
                if ( isAutoCommit() ) {
                    wsStatus.commit( ServerCommand.class.getSimpleName() );
                }
                
                print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.ServerCommand.VdbCopyToRepoFinished));
                
            } else {
                throw new InvalidCommandArgumentException(0, Messages.getString(Messages.ServerCommand.InvalidSubCommand));
            }
        } catch (InvalidCommandArgumentException e) {
            throw e;
        } catch (Exception e) {
            print( MESSAGE_INDENT, Messages.getString( Messages.ServerCommand.Failure, e.getLocalizedMessage() ) );
            return false;
        }
        return true;
    }

	private void writeToFile(String fileName, String content) {
	    BufferedWriter writer = null;
	    try
	    {
	        writer = new BufferedWriter(new FileWriter(fileName));
	        writer.write(content);

	    }
	    catch ( IOException e)
	    {
	    }
	    finally
	    {
	        try
	        {
	            if ( writer != null)
	                writer.close( );
	        }
	        catch ( IOException e)
	        {
	        }
	    }
	}
	
    /**
     * Displays the server objects with the specified name
     * @throws Exception
     */
    private void printServerObjectNames( WorkspaceStatus wsStatus,
                                 String objType ) throws Exception {

        // Print title
        final String title = Messages.getString( Messages.ServerCommand.ServerTypeHeader, getWorkspaceStatus().getTeiid().getName(wsStatus.getTransaction()), objType ); 
        print( MESSAGE_INDENT, title );
        
        List<String> objNames = getServerObjectNames(objType);
        PrintUtils.printList(this, objNames, Messages.getString( Messages.ServerCommand.ObjectNameHeader ));

        print();
    }
    
    /**
     * Displays the server objects with the specified name
     * @throws Exception
     */
    private void printServerObjectDetails( WorkspaceStatus wsStatus,
                                 String objType, String objName ) throws Exception {
        
        Teiid teiid = wsStatus.getTeiid();
        TeiidInstance teiidInstance = teiid.getTeiidInstance(wsStatus.getTransaction());
                
        // print header
        final String header = Messages.getString( Messages.ServerCommand.ServerObjDetailsHeader, teiid.getName(wsStatus.getTransaction()), objType, objName ); 
        print( MESSAGE_INDENT, header );
        
        if(objType.equalsIgnoreCase(VDB)) {
            TeiidVdb vdb = teiidInstance.getVdb(objName);
            if(vdb == null) {
                print( MESSAGE_INDENT, Messages.getString( Messages.ServerCommand.ServerItemNotFound, objName ) ); 
                return;
            }
            // print Vdb details
            printVdbDetails(vdb);
        } else if(objType.equalsIgnoreCase(TRANSLATOR)) {
            TeiidTranslator translator = teiidInstance.getTranslator(objName);
            if(translator == null) {
                print( MESSAGE_INDENT, Messages.getString( Messages.ServerCommand.ServerItemNotFound, objName ) ); 
                return;
            }
            // print Translator details
            printTranslatorDetails(translator);
        } else if(objType.equalsIgnoreCase(DATASOURCE)) {
            TeiidDataSource source = teiidInstance.getDataSource(objName);
            if(source == null) {
                print( MESSAGE_INDENT, Messages.getString( Messages.ServerCommand.ServerItemNotFound, objName ) ); 
                return;
            }
            // print DataSource details
            printDataSourceDetails(source);
        } else if(objType.equalsIgnoreCase(DATASOURCE_TYPE)) {
            Collection<TeiidPropertyDefinition> propDefns = teiidInstance.getTemplatePropertyDefns(objName);
            if(propDefns == null) {
                print( MESSAGE_INDENT, Messages.getString( Messages.ServerCommand.ServerItemNotFound, objName ) ); 
                return;
            }
            // print Template details
            printTemplateDetails(propDefns,objName);
        }
        
    }
    
    private void printVdbDetails(TeiidVdb vdb) {
        print(MESSAGE_INDENT, "Name   : "+vdb.getName()); //$NON-NLS-1$
        print(MESSAGE_INDENT, "Version: "+vdb.getVersion()); //$NON-NLS-1$
        
        print();
        print(MESSAGE_INDENT, "VDB Properties:"); //$NON-NLS-1$
        
        String nameTitle = Messages.getString( SHELL.PROPERTY_NAME_HEADER );
        String valueTitle = Messages.getString( SHELL.PROPERTY_VALUE_HEADER );
        
        PrintUtils.printProperties(this, vdb.getProperties(), nameTitle, valueTitle);
        
        print();
        print(MESSAGE_INDENT, "Models in the VDB:"); //$NON-NLS-1$
        
        Collection<String> vdbModels = vdb.getModelNames();
        if(vdbModels.isEmpty()) {
            print(MESSAGE_INDENT, "No models in this VDB"); //$NON-NLS-1$
        } else {
            List modelList = null;
            if(vdbModels instanceof List) {
                modelList = (List)vdbModels;
            } else {
                modelList = new ArrayList<String>(vdbModels);
            }
            PrintUtils.printList(this, modelList,Messages.getString( SHELL.PROPERTY_NAME_HEADER ));
        }
        print();
    }
    
    private void printTranslatorDetails(TeiidTranslator translator) {
        print(MESSAGE_INDENT, "Name: "+translator.getName()); //$NON-NLS-1$
        //print(MESSAGE_INDENT, "Type: "+translator.getType()); //$NON-NLS-1$
        
        print();
        print(MESSAGE_INDENT, "Translator Properties:"); //$NON-NLS-1$
        String nameTitle = Messages.getString( SHELL.PROPERTY_NAME_HEADER );
        String valueTitle = Messages.getString( SHELL.PROPERTY_VALUE_HEADER );
        PrintUtils.printProperties(this, translator.getProperties(), nameTitle, valueTitle);
        
        print();
    }
    
    private void printDataSourceDetails(TeiidDataSource dataSource) {
        print(MESSAGE_INDENT, "Name         : "+dataSource.getName()); //$NON-NLS-1$
        print(MESSAGE_INDENT, "Display Name : "+dataSource.getDisplayName()); //$NON-NLS-1$
        print(MESSAGE_INDENT, "Type         : "+dataSource.getType()); //$NON-NLS-1$
        
        print();
        print(MESSAGE_INDENT, "DataSource Properties:"); //$NON-NLS-1$
        String nameTitle = Messages.getString( SHELL.PROPERTY_NAME_HEADER );
        String valueTitle = Messages.getString( SHELL.PROPERTY_VALUE_HEADER );
        PrintUtils.printProperties(this, dataSource.getProperties(), nameTitle, valueTitle);
        
        print();
    }
    
    private void printTemplateDetails(Collection<TeiidPropertyDefinition> propDefns, String objName) {
        print(MESSAGE_INDENT, "Name : "+objName); //$NON-NLS-1$
        
        Properties props = new Properties();
        for(TeiidPropertyDefinition propDefn : propDefns) {
            String propName = propDefn.getName();
            Object propValue = propDefn.getDefaultValue();
            if(propValue==null || StringUtils.isEmpty(propValue.toString())) {
                propValue = "<no default>"; //$NON-NLS-1$
            }
            props.put(propName, propValue);
        }
        
        print();
        print(MESSAGE_INDENT, "DataSource Type Default Properties:"); //$NON-NLS-1$
        String nameTitle = Messages.getString( SHELL.PROPERTY_NAME_HEADER );
        String valueTitle = "DEFAULT VALUE"; //$NON-NLS-1$
        PrintUtils.printProperties(this, props, nameTitle, valueTitle);
        
        print();
    }
    
    /**
     * Determine if the default teiid instance is connected
     * @param wsStatus
     *        the workspace status
     * @return <code>true</code> if the default teiid instance is defined and connected.
     * @throws Exception
     *         if an error occurs
     */
    protected static boolean isTeiidConnected( final WorkspaceStatus wsStatus ) throws Exception {
        Teiid teiid = wsStatus.getTeiid();

        if (teiid == null) {
            return false;
        }

        TeiidInstance teiidInstance = teiid.getTeiidInstance(wsStatus.getTransaction());
        return teiidInstance.isConnected();
    }

    /**
     * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion(String lastArgument, List<CharSequence> candidates) throws Exception {

    	if (getArguments().isEmpty()) {
    		// --------------------------------------------------------------
    		// No arg - offer subcommands
    		// --------------------------------------------------------------
    		if(lastArgument==null) {
    			candidates.addAll(SUBCMDS);
                return 0;
    		}

            for ( final String item : SUBCMDS ) {
                if ( item.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) {
                    candidates.add( item );
                }
            }

            return ( candidates.isEmpty() ? -1 : ( toString().length() + 1 ) );
    	}

    	if (getArguments().size()==1) {
    		String cmdArgLower = getArguments().get(0).toLowerCase();

    		if(SUBCMD_SHOW.equals(cmdArgLower)) {
                if(lastArgument==null) {
                    candidates.addAll(SERVER_OBJ_TYPES_ALL);
                    return 0;
                }

                for ( final String item : SERVER_OBJ_TYPES_ALL ) {
                    if ( item.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) {
                        candidates.add( item );
                    }
                }
    		} else if(SUBCMD_DEPLOY.equals(cmdArgLower) || SUBCMD_UNDEPLOY.equals(cmdArgLower)) {
                if(lastArgument==null) {
                    candidates.addAll(SERVER_OBJ_DEPLOY_TYPES);
                    return 0;
                }

                for ( final String item : SERVER_OBJ_DEPLOY_TYPES ) {
                    if ( item.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) {
                        candidates.add( item );
                    }
                }
            } else if(SUBCMD_IMPORTVDB.equals(cmdArgLower)) {
                List<String> items = this.getServerObjectNames(VDB);
                if(lastArgument==null) {
                    candidates.addAll(items);
                    return 0;
                }

                for ( final String item : items ) {
                    if ( item.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) {
                        candidates.add( item );
                    }
                }
            } else if(SUBCMD_SET.equals(cmdArgLower)) {
                WorkspaceStatus wStatus = getWorkspaceStatus();
                WorkspaceManager wkspManager = getWorkspaceStatus().getCurrentContext().getWorkspaceManager();
                List<Teiid> teiids = wkspManager.findTeiids(wStatus.getTransaction());
                List<String> teiidNames = new ArrayList<String>(teiids.size());
                for(Teiid teiid : teiids) {
                    teiidNames.add(teiid.getName(wStatus.getTransaction()));
                }
                if(lastArgument==null) {
                    candidates.addAll(teiidNames);
                } else {
                    for (String theName : teiidNames) {
                        if (theName.startsWith(lastArgument.toLowerCase())) {
                            candidates.add(theName);
                        }
                    }
                }
            }
            
            return ( candidates.isEmpty() ? -1 : ( toString().length() + 1 ) );
    	}
    	
        if (getArguments().size()==2) {
            String cmdArgLower = getArguments().get(0).toLowerCase();
            String arg2 = getArguments().get(1);

            if(SUBCMD_UNDEPLOY.equals(cmdArgLower) || SUBCMD_SHOW.equals(cmdArgLower)) {
                List<String> items = this.getServerObjectNames(arg2);
                if(lastArgument==null) {
                    candidates.addAll(items);
                    return 0;
                }

                for ( final String item : items ) {
                    if ( item.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) {
                        candidates.add( item );
                    }
                }
                return ( candidates.isEmpty() ? -1 : ( toString().length() + 1 ) );
            }
            
            if(SUBCMD_DEPLOY.equals(cmdArgLower)) {
                // No arg currently - provide all options
                if(StringUtils.isBlank(lastArgument)) {
                    // Get all path options for the type
                    String[] fullPaths = null;
                    if ( arg2.toLowerCase().equals( VDB ) ) {
                        // find all VDB paths
                        fullPaths = FindCommand.query( getWorkspaceStatus(), KomodoType.VDB, null, null );
                    } else if ( arg2.toLowerCase().equals( DATASOURCE )) {
                        fullPaths = new String[0];
                    }
                    for( final String path : fullPaths ) {
                        candidates.add(path);
                    }
                } else {
                // Partial path supplied - get options for it
                    WorkspaceContext currentContext = getWorkspaceStatus().getCurrentContext();

                    // The arg is expected to be a path
                    updateTabCompleteCandidatesForPath(candidates, currentContext, true, lastArgument);
                }

                return ( candidates.isEmpty() ? -1 : ( StringUtils.isBlank( lastArgument ) ? 0 : ( toString().length()) ) );
            }
        }
    	
    	return -1;
    }

    private List<String> getServerObjectNames(String serverObjType) throws Exception {
        List<String> objNames = new ArrayList<String>();
        
        Teiid teiid = getWorkspaceStatus().getTeiid();

        if (teiid != null) {
            TeiidInstance teiidInstance = teiid.getTeiidInstance(getWorkspaceStatus().getTransaction());
            if(serverObjType.equalsIgnoreCase(VDB) || serverObjType.equalsIgnoreCase(VDBS)) {
                Collection<TeiidVdb> vdbs = teiidInstance.getVdbs();
                for(TeiidVdb vdb : vdbs) {
                    String name = vdb.getName();
                    objNames.add(name);
                }
            } else if(serverObjType.equalsIgnoreCase(TRANSLATOR) || serverObjType.equalsIgnoreCase(TRANSLATORS)) {
                Collection<TeiidTranslator> translators = teiidInstance.getTranslators();
                for(TeiidTranslator translator : translators) {
                    String name = translator.getName();
                    objNames.add(name);
                }
            } else if(serverObjType.equalsIgnoreCase(DATASOURCE) || serverObjType.equalsIgnoreCase(DATASOURCES)) {
                Collection<TeiidDataSource> sources = teiidInstance.getDataSources();
                for(TeiidDataSource source : sources) {
                    String name = source.getDisplayName();
                    objNames.add(name);
                }
            } else if(serverObjType.equalsIgnoreCase(DATASOURCE_TYPE) || serverObjType.equalsIgnoreCase(DATASOURCE_TYPES)) {
                Set<String> types = teiidInstance.getDataSourceTypeNames();
                for(String type : types) {
                    objNames.add(type);
                }
            }
        }

        return objNames;
    }

}
