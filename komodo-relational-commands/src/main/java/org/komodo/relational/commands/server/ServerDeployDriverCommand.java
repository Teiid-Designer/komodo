/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.server;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.io.File;
import java.util.Set;
import org.komodo.relational.commands.workspace.WorkspaceCommandsI18n;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to deploy a driver from the file system to the connected server.
 */
public final class ServerDeployDriverCommand extends ServerShellCommand {

    static final String NAME = "server-deploy-driver"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ServerDeployDriverCommand( final WorkspaceStatus status ) {
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
            final String driverName = requiredArgument( 0, I18n.bind( ServerCommandsI18n.missingDriverNameForDeployment ) );
            final String fileName = requiredArgument( 1, I18n.bind( ServerCommandsI18n.missingInputDriverFilePath ) );

            { // Validates the supplied fileNameArg is a valid, readable file
                final String validationResult = validateReadableFileArg( fileName );

                if ( !CompletionConstants.OK.equals( validationResult ) ) {
                    return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.inputFileError, fileName, validationResult ), null );
                }
            }

            // Validates that a server is connected
            CommandResult validationResult = validateHasConnectedWorkspaceServer();
            if ( !validationResult.isOk() ) {
                return validationResult;
            }

            // Deploy the driver to the server
            try {
                // Determine if the server already has a type with the requested name
                Set< String > serverTypes = getWorkspaceTeiidInstance().getDataSourceTypeNames();
                if(serverTypes.contains(driverName)) {
                    return new CommandResultImpl( false, I18n.bind( ServerCommandsI18n.driverDeployErrorServerHasMatch, driverName ), null );
                }
                
                File driverFile = new File(fileName);
                final TeiidInstance teiidInstance = getWorkspaceTeiidInstance();
                try {
                    teiidInstance.deployDriver(driverName, driverFile);
                } catch (Exception ex) {
                    result = new CommandResultImpl( false, I18n.bind( ServerCommandsI18n.driverDeploymentError, ex.getLocalizedMessage() ), null );
                    return result;
                }
                
            } catch (Exception ex) {
                result = new CommandResultImpl( false, I18n.bind( ServerCommandsI18n.connectionErrorWillDisconnect ), ex );
                WkspStatusServerManager.getInstance(getWorkspaceStatus()).disconnectDefaultServer();
                return result;
            }

            print( MESSAGE_INDENT, I18n.bind(ServerCommandsI18n.driverDeployFinished, driverName) );
            result = CommandResult.SUCCESS;
        } catch ( final Exception e ) {
            result = new CommandResultImpl( e );
        }

        return result;
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverDeployDriverHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverDeployDriverExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverDeployDriverUsage ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 2;
    }

}
