/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.modelsource;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for @{link {@link ModelSource model source}-related shell commands.
 */
abstract class ModelSourceShellCommand extends RelationalShellCommand {

    protected static final String JNDI_NAME = "sourceJndiName"; //$NON-NLS-1$
    protected static final String TRANSLATOR_NAME = "sourceTranslator"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { JNDI_NAME, TRANSLATOR_NAME } );

    protected ModelSourceShellCommand( final String name,
                                       final WorkspaceStatus status ) {
        super( status, name );
    }

    protected ModelSource getModelSource() throws Exception {
        assert getContext() instanceof ModelSource;
        return ModelSource.RESOLVER.resolve(getTransaction(), getContext());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return ModelSource.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            return false;
        }
    }

}
