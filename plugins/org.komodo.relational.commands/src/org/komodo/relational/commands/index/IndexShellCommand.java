/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.index;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.model.Index;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for @{link {@link Index Index}-related shell commands.
 */
abstract class IndexShellCommand extends RelationalShellCommand {

    protected static final String EXPRESSION = "expression"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { EXPRESSION } );

    protected IndexShellCommand( final String name,
                                 final WorkspaceStatus status ) {
        super( status, name );
    }

    protected Index getIndex() throws Exception {
        assert getContext() instanceof Index;
        return Index.RESOLVER.resolve(getTransaction(), getContext());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return Index.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            return false;
        }
    }

}
