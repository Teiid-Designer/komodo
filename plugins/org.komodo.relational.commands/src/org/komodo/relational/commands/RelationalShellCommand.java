/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands;

import org.komodo.relational.RelationalObject;
import org.komodo.relational.commands.workspace.WorkspaceCommandsI18n;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.utils.i18n.I18n;

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

        throw new KException( I18n.bind( WorkspaceCommandsI18n.invalidObjectType, kobject.getAbsolutePath() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getCategory()
     */
    @Override
    public String getCategory() {
        return I18n.bind( RelationalCommandsI18n.relationalCommandCategory );
    }

    protected ShellCommand getCommand( final String commandName ) throws Exception {
        return getWorkspaceStatus().getCommand( commandName );
    }

    protected String getDisplayType() throws Exception {
        return getDisplayType( get() );
    }

    protected String getDisplayType( final KomodoObject kobject ) throws Exception {
        if ( kobject instanceof RelationalObject ) {
            return getWorkspaceStatus().getLabelProvider().getTypeDisplay(getWorkspaceStatus().getTransaction(), kobject);
        }

        throw new KException( I18n.bind( WorkspaceCommandsI18n.invalidObjectType, kobject.getAbsolutePath() ) );
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

}
