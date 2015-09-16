/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model.virtualprocedure;

import org.komodo.relational.Messages;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.model.VirtualProcedure;
import org.komodo.relational.model.internal.VirtualProcedureImpl;
import org.komodo.shell.api.WorkspaceStatus;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

/**
 * A base class for @{link {@link VirtualProcedure VirtualProcedure}-related shell commands.
 */
abstract class VirtualProcedureShellCommand extends RelationalShellCommand {

    protected VirtualProcedureShellCommand( final String name,
                               final boolean shouldCommit,
                               final WorkspaceStatus status ) {
        super( status, shouldCommit, name );
    }

    protected VirtualProcedure getVirtualProcedure() throws Exception {
        return new VirtualProcedureImpl( getTransaction(), getRepository(), getPath() );
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
            isValid = isCurrentTypeValid( TeiidDdlLexicon.CreateProcedure.PROCEDURE_STATEMENT );
        } catch (Exception ex) {
            // exception returns false
        }
        return isValid;
    }

    @Override
    protected String getMessage(Enum< ? > key, Object... parameters) {
        return Messages.getString(VirtualProcedureCommandMessages.RESOURCE_BUNDLE,key.toString(),parameters);
    }
    
    /**
     * @see org.komodo.shell.api.ShellCommand#printHelp(int indent)
     */
    @Override
    public void printHelp( final int indent ) {
        print( indent, Messages.getString( VirtualProcedureCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".help" ) ); //$NON-NLS-1$
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#printUsage(int indent)
     */
    @Override
    public void printUsage( final int indent ) {
        print( indent, Messages.getString( VirtualProcedureCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".usage" ) ); //$NON-NLS-1$
    }

}
