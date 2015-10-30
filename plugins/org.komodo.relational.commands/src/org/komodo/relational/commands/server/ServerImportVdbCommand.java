/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.server;

import static org.komodo.relational.commands.server.ServerCommandMessages.Common.MissingVdbName;
import static org.komodo.relational.commands.server.ServerCommandMessages.Common.ServerVdbNotFound;
import static org.komodo.relational.commands.server.ServerCommandMessages.ServerImportVdbCommand.CanOnlyCopyDynamicVDBs;
import static org.komodo.relational.commands.server.ServerCommandMessages.ServerImportVdbCommand.VdbCopyToRepoFinished;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.komodo.relational.commands.workspace.UploadVdbCommand;
import org.komodo.relational.teiid.Teiid;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidVdb;

/**
 * A shell command to import a server vdb into the workspace
 */
public final class ServerImportVdbCommand extends ServerShellCommand {

    static final String NAME = "server-import-vdb"; //$NON-NLS-1$

    private static final String TEMPFILE_PREFIX = "Vdb-"; //$NON-NLS-1$
    private static final String TEMPFILE_SUFFIX = ".xml"; //$NON-NLS-1$
    
    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ServerImportVdbCommand( final WorkspaceStatus status ) {
        super( NAME, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        CommandResult result = null;

        try {
            String vdbName = requiredArgument( 0, getMessage( MissingVdbName ) );
            
            // Validates that a server is connected
            CommandResult validationResult = validateHasConnectedWorkspaceServer();
            if ( !validationResult.isOk() ) {
                return validationResult;
            }

            // Undeploy the VDB
            Teiid teiid = getWorkspaceServer();
            TeiidInstance teiidInstance = teiid.getTeiidInstance(getTransaction());
            
            // Get the VDB - make sure its a dynamic VDB
            TeiidVdb vdb = teiidInstance.getVdb(vdbName);
            if(vdb == null) {
                return new CommandResultImpl( false, getMessage(ServerVdbNotFound), null );
            }
            if(!vdb.isXmlDeployment()) {
                return new CommandResultImpl( false, getMessage(CanOnlyCopyDynamicVDBs), null );
            }
            
            // Export the string content
            String vdbStr = vdb.export();
            
            // Output the content to a temp file
            File tempFile = File.createTempFile(TEMPFILE_PREFIX, TEMPFILE_SUFFIX);
            writeToFile(tempFile.getPath(),vdbStr);
            
            // Upload the VdbFile
            UploadVdbCommand uploadVdbCommand = new UploadVdbCommand(getWorkspaceStatus());
            uploadVdbCommand.setArguments(new Arguments( vdbName + StringConstants.SPACE + tempFile.getAbsolutePath() )); 
            CommandResult uploadResult = uploadVdbCommand.execute();
            if(!uploadResult.isOk()) {
                return uploadResult;
            }
            
            print( MESSAGE_INDENT, getMessage(VdbCopyToRepoFinished) );
            result = CommandResult.SUCCESS;
        } catch ( final Exception e ) {
            result = new CommandResultImpl( e );
        }

        return result;
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 1;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        return (isWorkspaceContext() && hasConnectedWorkspaceServer());
    }
    
    private boolean isWorkspaceContext() {
        try {
            final KomodoType contextType = getContext().getTypeIdentifier( getTransaction() );
            return ( contextType == KomodoType.WORKSPACE );
        } catch ( final Exception e ) {
            return false;
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        final Arguments args = getArguments();

        Collection<String> existingVdbNames = getDeployedVdbs();

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( existingVdbNames );
            } else {
                for ( final String item : existingVdbNames ) {
                    if ( item.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) {
                        candidates.add( item );
                    }
                }
            }

            return 0;
        }

        // no tab completion
        return -1;
    }
    
    /*
     * Return the deployed vdbs on the workspace server
     */
    private Collection<String> getDeployedVdbs() throws Exception {
        Teiid teiid = getWorkspaceServer();
        List< String > existingVdbNames = new ArrayList< String >();
        Collection< TeiidVdb > vdbs = teiid.getTeiidInstance( getTransaction() ).getVdbs();
        for ( TeiidVdb vdb : vdbs ) {
            String name = vdb.getName();
            existingVdbNames.add( name );
        }
        return existingVdbNames;
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
    
}
