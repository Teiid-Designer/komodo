/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.entry;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.vdb.Entry;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for {@link Entry VDB entry}-related shell commands.
 */
abstract class EntryShellCommand extends RelationalShellCommand {

    protected static final String DESCRIPTION = "description"; //$NON-NLS-1$
    protected static final String PATH = "path"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { DESCRIPTION, PATH } );

    protected EntryShellCommand( final String name,
                                 final WorkspaceStatus status ) {
        super( status, name );
    }

    protected Entry getEntry() throws Exception {
        assert getContext() instanceof Entry;
        return Entry.RESOLVER.resolve(getTransaction(), getContext());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return Entry.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            return false;
        }
    }

}
