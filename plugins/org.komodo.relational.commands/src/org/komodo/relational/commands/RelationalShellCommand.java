/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_OBJECT_TYPE;
import org.komodo.relational.Messages;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

/**
 * A base class for shell command's relating to relational objects.
 */
public abstract class RelationalShellCommand extends BuiltInShellCommand {

    protected RelationalShellCommand( final WorkspaceStatus wsStatus,
                                      final String... commandNames ) {
        super( wsStatus, commandNames );
    }

    protected RelationalObject get() throws Exception {
        final KomodoObject kobject = getWorkspaceStatus().getCurrentContext();

        if ( kobject instanceof RelationalObject ) {
            return ( RelationalObject )kobject;
        }

        throw new KException( getMessage(INVALID_OBJECT_TYPE, kobject.getAbsolutePath() ) );
    }

    protected ShellCommand getCommand( final String commandName ) throws Exception {
        return getWorkspaceStatus().getCommand( commandName );
    }

    protected String getDisplayType() throws Exception {
        return getDisplayType( get() );
    }

    protected String getDisplayType( final KomodoObject kobject ) throws Exception {
        if ( kobject instanceof RelationalObject ) {
            return ( ( RelationalObject )kobject ).getTypeDisplayName();
        }

        throw new KException( getMessage(INVALID_OBJECT_TYPE, kobject.getAbsolutePath() ) );
    }

    protected String getPath() throws Exception {
        return getWorkspaceStatus().getCurrentContext().getAbsolutePath();
    }

    protected Repository getRepository() throws KException {
        return getWorkspaceStatus().getCurrentContext().getRepository();
    }

    protected WorkspaceManager getWorkspaceManager() throws KException {
        return WorkspaceManager.getInstance( getRepository() );
    }

    protected String getMessage(Enum< ? > key, Object... parameters) {
        return Messages.getString(WorkspaceCommandMessages.RESOURCE_BUNDLE,key.toString(),parameters);
    }

    protected String getWorkspaceMessage(Enum< ? > key, Object... parameters) {
        return Messages.getString(WorkspaceCommandMessages.RESOURCE_BUNDLE,key.toString(),parameters);
    }

}
