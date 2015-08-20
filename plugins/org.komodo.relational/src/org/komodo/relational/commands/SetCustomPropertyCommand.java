/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.MISSING_PROPERTY_NAME_VALUE;
import org.komodo.relational.RelationalObject;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to create a VDB.
 */
public final class SetCustomPropertyCommand extends RelationalShellCommand {

    static final String NAME = "set-custom-property"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetCustomPropertyCommand( final WorkspaceStatus status ) {
        super( NAME, true, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String name = requiredArgument( 0, getMessage(MISSING_PROPERTY_NAME_VALUE));
        final String value = requiredArgument( 1, getMessage(MISSING_PROPERTY_NAME_VALUE));

        final RelationalObject robject = get();
        robject.setProperty( getTransaction(), name, value );

        return true;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
//        final int currType = getWorkspaceStatus().getWorkspaceContext().getKomodoObj().getTypeId();
        // TODO type needs to be a relational object here
        return true;
    }

}
