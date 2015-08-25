/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import static org.komodo.relational.commands.vdb.VdbCommandMessages.General.MISSING_IMPORT_NAME;
import org.komodo.relational.vdb.Vdb;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add a VDB import to a VDB.
 */
public final class AddImportCommand extends VdbShellCommand {

    static final String NAME = "add-import"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddImportCommand( final WorkspaceStatus status ) {
        super( NAME, true, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String vdbName = requiredArgument( 0, getMessage(MISSING_IMPORT_NAME) );

        final Vdb vdb = getVdb();
        vdb.addImport( getTransaction(), vdbName );

        return true;
    }

}
