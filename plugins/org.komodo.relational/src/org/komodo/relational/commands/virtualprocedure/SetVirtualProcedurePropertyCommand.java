/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.virtualprocedure;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_INTEGER_PROPERTY_VALUE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_PROPERTY_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.MISSING_PROPERTY_NAME_VALUE;
import static org.komodo.relational.commands.storedprocedure.StoredProcedureCommandMessages.General.INVALID_SCHEMA_ELEMENT_TYPE_PROPERTY_VALUE;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.Arrays;
import java.util.List;
import org.komodo.relational.model.SchemaElement;
import org.komodo.relational.model.VirtualProcedure;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.core.SetPropertyCommand;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to set VirtualProcedure properties
 */
public final class SetVirtualProcedurePropertyCommand extends VirtualProcedureShellCommand {

    static final String NAME = "set-virtual-procedure-property"; //$NON-NLS-1$

    private static final String AS_CLAUSE_STATEMENT = "as-clause-statement"; //$NON-NLS-1$
    private static final String DESCRIPTION = "description"; //$NON-NLS-1$
    private static final String NAME_IN_SOURCE = "name-in-source"; //$NON-NLS-1$
    private static final String SCHEMA_ELEMENT_TYPE = "schema-element-type"; //$NON-NLS-1$
    private static final String UPDATE_COUNT = "update-count"; //$NON-NLS-1$
    private static final String UUID = "uuid"; //$NON-NLS-1$

    private static final List< String > ALL_PROPS = Arrays.asList( new String[] { AS_CLAUSE_STATEMENT, DESCRIPTION, NAME_IN_SOURCE, 
                                                                                  SCHEMA_ELEMENT_TYPE, UPDATE_COUNT, UUID } );

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetVirtualProcedurePropertyCommand( final WorkspaceStatus status ) {
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

        final VirtualProcedure proc = getVirtualProcedure();
        final UnitOfWork transaction = getTransaction();
        boolean success = true;

        switch ( name ) {
            case AS_CLAUSE_STATEMENT:
                proc.setAsClauseStatement(getTransaction(), value);
                break;
            case DESCRIPTION:
                proc.setDescription(getTransaction(), value);
                break;
            case NAME_IN_SOURCE:
                proc.setNameInSource(getTransaction(), value);
                break;
            case SCHEMA_ELEMENT_TYPE:
                if ( SchemaElement.SchemaElementType.FOREIGN.name().equals( value ) ) {
                    proc.setSchemaElementType( transaction, SchemaElement.SchemaElementType.FOREIGN );
                } else if ( SchemaElement.SchemaElementType.VIRTUAL.name().equals( value ) ) {
                    proc.setSchemaElementType( transaction, SchemaElement.SchemaElementType.VIRTUAL );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_SCHEMA_ELEMENT_TYPE_PROPERTY_VALUE, SCHEMA_ELEMENT_TYPE ) );
                    success = false;
                }
                break;
            case UPDATE_COUNT:
                try {
                    final long count = Long.parseLong( value );
                    proc.setUpdateCount(transaction, count);
                } catch ( final NumberFormatException e ) {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_INTEGER_PROPERTY_VALUE, UPDATE_COUNT ) );
                    success = false;
                }

                break;
            case UUID:
                proc.setUuid(getTransaction(), value);
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

        if ( ( args.size() == 1 ) ) {
            String theArg = getArguments().get(0);
            if( SCHEMA_ELEMENT_TYPE.equals(theArg)) {
                candidates.add( SchemaElement.SchemaElementType.FOREIGN.name() );
                candidates.add( SchemaElement.SchemaElementType.VIRTUAL.name() );
            }
        }
        
        // no tab completion
        return -1;
    }

}
