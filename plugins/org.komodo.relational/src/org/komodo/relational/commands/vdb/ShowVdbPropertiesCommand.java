/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import org.komodo.relational.vdb.Vdb;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.PrintUtils;

/**
 * A shell command to show all VDB properties.
 */
public final class ShowVdbPropertiesCommand extends VdbShellCommand {

    static final String NAME = "show-vdb-properties"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ShowVdbPropertiesCommand( final WorkspaceStatus status ) {
        super( NAME, false, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final Vdb vdb = getVdb();
        
        WorkspaceStatus wsStatus = getWorkspaceStatus();
        // Print properties for the VDB
        PrintUtils.printProperties(wsStatus,wsStatus.isShowingHiddenProperties(),wsStatus.isShowingPropertyNamePrefixes(),vdb);

        print();
        return true;
    }

}
