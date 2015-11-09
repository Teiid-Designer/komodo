/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.table;

import static org.komodo.relational.commands.table.TableCommandMessages.ShowAccessPatternsCommand.ACCESS_PATTERNS_HEADER;
import static org.komodo.relational.commands.table.TableCommandMessages.ShowAccessPatternsCommand.NO_ACCESS_PATTERNS;
import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.General.PRINT_RELATIONAL_OBJECT;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.model.AccessPattern;
import org.komodo.relational.model.Table;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to show all the {@link AccessPattern access patterns} of a {@link Table}.
 */
public final class ShowAccessPatternsCommand extends TableShellCommand {

    static final String NAME = "show-access-patterns"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ShowAccessPatternsCommand( final WorkspaceStatus status ) {
        super( NAME, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        try {
            final Table table = getTable();
            final AccessPattern[] accessPatterns = table.getAccessPatterns( getTransaction() );

            if ( accessPatterns.length == 0 ) {
                print( MESSAGE_INDENT, getMessage( NO_ACCESS_PATTERNS, table.getName( getTransaction() ) ) );
            } else {
                print( MESSAGE_INDENT, getMessage( ACCESS_PATTERNS_HEADER, table.getName( getTransaction() ) ) );

                final int indent = (MESSAGE_INDENT * 2);

                for ( final AccessPattern accessPattern : accessPatterns ) {
                    print( indent,
                           getWorkspaceMessage( PRINT_RELATIONAL_OBJECT,
                                                accessPattern.getName( getTransaction() ),
                                                accessPattern.getTypeDisplayName() ) );
                }
            }

            return CommandResult.SUCCESS;
        } catch ( final Exception e ) {
            return  new CommandResultImpl( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 0;
    }

}
