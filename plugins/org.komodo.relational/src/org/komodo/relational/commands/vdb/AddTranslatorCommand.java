/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.General.MISSING_TRANSLATOR_NAME;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.General.MISSING_TRANSLATOR_TYPE;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.AddTranslatorCommand.TRANSLATOR_ADDED;
import org.komodo.relational.vdb.Vdb;
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
        super( NAME, true, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#execute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String translatorName = requiredArgument( 0, getMessage(MISSING_TRANSLATOR_NAME) );
        final String translatorType = requiredArgument( 1, getMessage(MISSING_TRANSLATOR_TYPE) );

        final Vdb vdb = getVdb();
        vdb.addTranslator( getTransaction(), translatorName, translatorType );

        // Print success message
        print(MESSAGE_INDENT, getMessage(TRANSLATOR_ADDED,translatorName));
        
        return true;
    }

}
