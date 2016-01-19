/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.condition;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.vdb.Condition;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for @{link {@link Condition Condition}-related shell commands.
 */
abstract class ConditionShellCommand extends RelationalShellCommand {

    protected static final String CONSTRAINT = "constraint"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { CONSTRAINT } );

    protected ConditionShellCommand( final String name,
                                     final WorkspaceStatus status ) {
        super( status, name );
    }

    protected Condition getCondition() throws Exception {
        assert getContext() instanceof Condition;
        return Condition.RESOLVER.resolve(getTransaction(), getContext());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return Condition.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            // exception returns false
        }
        return false;
    }

}
