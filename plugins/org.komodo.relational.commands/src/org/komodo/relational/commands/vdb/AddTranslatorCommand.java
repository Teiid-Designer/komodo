/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import static org.komodo.relational.commands.vdb.VdbCommandMessages.AddTranslatorCommand.TRANSLATOR_ADDED;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.General.MISSING_TRANSLATOR_NAME;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.General.MISSING_TRANSLATOR_TYPE;
import org.komodo.relational.vdb.Vdb;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add a translator to a VDB.
 */
public class AddTranslatorCommand extends VdbShellCommand {

    static final String NAME = "add-translator"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddTranslatorCommand( final WorkspaceStatus status ) {
        super( NAME, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        CommandResult result = null;

        try {
            final String translatorName = requiredArgument( 0, getMessage( MISSING_TRANSLATOR_NAME ) );
            final String translatorType = requiredArgument( 1, getMessage( MISSING_TRANSLATOR_TYPE ) );

            final Vdb vdb = getVdb();
            vdb.addTranslator( getTransaction(), translatorName, translatorType );

            result = new CommandResultImpl( getMessage( TRANSLATOR_ADDED, translatorName ) );
        } catch ( final Exception e ) {
            result = new CommandResultImpl( e );
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 2;
    }

}
