/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.mask;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.vdb.Mask;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for @{link {@link Mask Mask}-related shell commands.
 */
abstract class MaskShellCommand extends RelationalShellCommand {

    protected static final String ORDER = "order"; //$NON-NLS-1$
    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { ORDER } );

    protected MaskShellCommand( final String name,
                                final WorkspaceStatus status ) {
        super( status, name );
    }

    protected Mask getMask() throws Exception {
        assert getContext() instanceof Mask;
        return Mask.RESOLVER.resolve(getTransaction(), getContext());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return Mask.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            return false;
        }
    }

}
