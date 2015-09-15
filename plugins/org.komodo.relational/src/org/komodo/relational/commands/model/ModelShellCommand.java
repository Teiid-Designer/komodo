/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model;

import org.komodo.relational.Messages;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.internal.ModelImpl;
import org.komodo.shell.api.WorkspaceStatus;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * A base class for @{link {@link Model Model}-related shell commands.
 */
abstract class ModelShellCommand extends RelationalShellCommand {

    protected ModelShellCommand( final String name,
                               final boolean shouldCommit,
                               final WorkspaceStatus status ) {
        super( status, shouldCommit, name );
    }

    protected Model getModel() throws Exception {
        return new ModelImpl( getTransaction(), getRepository(), getPath() );
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
            isValid = isCurrentTypeValid( VdbLexicon.Vdb.DECLARATIVE_MODEL );
        } catch (Exception ex) {
            // exception returns false
        }
        return isValid;
    }

    @Override
    protected String getMessage(Enum< ? > key, Object... parameters) {
        return Messages.getString(ModelCommandMessages.RESOURCE_BUNDLE,key.toString(),parameters);
    }
    
    /**
     * @see org.komodo.shell.api.ShellCommand#printHelp(int indent)
     */
    @Override
    public void printHelp( final int indent ) {
        print( indent, Messages.getString( ModelCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".help" ) ); //$NON-NLS-1$
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#printUsage(int indent)
     */
    @Override
    public void printUsage( final int indent ) {
        print( indent, Messages.getString( ModelCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".usage" ) ); //$NON-NLS-1$
    }

}
