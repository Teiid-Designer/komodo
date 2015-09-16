/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model.table.foreignkey;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_PROPERTY_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.MISSING_PROPERTY_NAME_VALUE;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.Arrays;
import java.util.List;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.model.Column.Searchable;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.core.SetPropertyCommand;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to set Column properties
 */
public final class SetForeignKeyPropertyCommand extends ForeignKeyShellCommand {

    static final String NAME = "set-foreignkey-property"; //$NON-NLS-1$

    private static final String PRIMARY_TYPE = "primary-type"; //$NON-NLS-1$

    private static final List< String > ALL_PROPS = Arrays.asList( new String[] { PRIMARY_TYPE } );

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetForeignKeyPropertyCommand( final WorkspaceStatus status ) {
        super( NAME, true, status );
        // Overrides the BuiltInCommand "set-property"
        setOverriddenCommands(new String[]{SetPropertyCommand.NAME});
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String name = requiredArgument( 0, getWorkspaceMessage(MISSING_PROPERTY_NAME_VALUE) );
        final String value = requiredArgument( 1, getWorkspaceMessage(MISSING_PROPERTY_NAME_VALUE) );

//        final ForeignKey fk = getForeignKey();
//        fk.setPrimaryType(transaction, typeName);
//        fk.setReferencesTable(transaction, newReferencesTable);
        
        final UnitOfWork transaction = getTransaction();
        boolean success = true;

        switch ( name ) {
            case PRIMARY_TYPE:
//                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
//                    column.setAutoIncremented( transaction, Boolean.parseBoolean( value ) );
//                } else {
//                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, AUTO_INCREMENTED ) );
//                    success = false;
//                }
//
                break;
            default:
                success = false;
                print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_PROPERTY_NAME, NAME ) );
                break;
        }

        return success;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        final Arguments args = getArguments();

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( ALL_PROPS );
            } else {
                for ( final String item : ALL_PROPS ) {
                    if ( item.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) {
                        candidates.add( item );
                    }
                }
            }

            return 0;
        }

//        if ( ( args.size() == 1 ) ) {
//            String theArg = getArguments().get(0);
//            if( AUTO_INCREMENTED.equals(theArg) || CASE_SENSITIVE.equals(theArg) || CURRENCY.equals(theArg)
//             || FIXED_LENGTH.equals(theArg) || SELECTABLE.equals(theArg) || UPDATABLE.equals(theArg)) {
//                candidates.add( Boolean.TRUE.toString() );
//                candidates.add( Boolean.FALSE.toString() );
//            } else if( NULLABLE.equals(theArg) ) {
//                candidates.add( Nullable.NO_NULLS.name() );
//                candidates.add( Nullable.NULLABLE.name() );
//                candidates.add( Nullable.NULLABLE_UNKNOWN.name() );
//            } else if( SEARCHABLE.equals(theArg)) {
//                candidates.add( Searchable.ALL_EXCEPT_LIKE.name() );
//                candidates.add( Searchable.LIKE_ONLY.name() );
//                candidates.add( Searchable.SEARCHABLE.name() );
//                candidates.add( Searchable.UNSEARCHABLE.name() );
//            }
//
//            return 0;
//        }
        
        // no tab completion
        return -1;
    }

}
