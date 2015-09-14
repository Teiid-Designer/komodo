/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb.datarole;

import org.komodo.relational.Messages;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.internal.DataRoleImpl;
import org.komodo.shell.api.WorkspaceStatus;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * A base class for {@link DataRole data role}-related shell commands.
 */
abstract class DataRoleShellCommand extends RelationalShellCommand {

    protected DataRoleShellCommand( final String name,
                                    final boolean shouldCommit,
                                    final WorkspaceStatus status ) {
        super( status, shouldCommit, name );
    }

    protected DataRole getDataRole() throws Exception {
        return new DataRoleImpl( getTransaction(), getRepository(), getPath() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        boolean isValid = false;
        try {
            isValid = isCurrentTypeValid( VdbLexicon.DataRole.DATA_ROLE );
        } catch (Exception ex) {
            // exception returns false
        }
        return isValid;
    }

    @Override
    protected String getMessage(Enum< ? > key, Object... parameters) {
        return Messages.getString(DataRoleCommandMessages.RESOURCE_BUNDLE,key.toString(),parameters);
    }
    
    /**
     * @see org.komodo.shell.api.ShellCommand#printHelp(int indent)
     */
    @Override
    public void printHelp( final int indent ) {
        print( indent, Messages.getString( DataRoleCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".help" ) ); //$NON-NLS-1$
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#printUsage(int indent)
     */
    @Override
    public void printUsage( final int indent ) {
        print( indent, Messages.getString( DataRoleCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".usage" ) ); //$NON-NLS-1$
    }

}
