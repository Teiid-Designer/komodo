/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.column;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.Messages;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.internal.ColumnImpl;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for @{link {@link Column Column}-related shell commands.
 */
abstract class ColumnShellCommand extends RelationalShellCommand {

    protected static final String AUTO_INCREMENTED = "auto-incremented"; //$NON-NLS-1$
    protected static final String CASE_SENSITIVE = "case-sensitive"; //$NON-NLS-1$
    protected static final String CHAR_OCTET_LENGTH = "char-octet-lenght"; //$NON-NLS-1$
    protected static final String COLLATION_NAME = "collation-name"; //$NON-NLS-1$
    protected static final String CURRENCY = "currency"; //$NON-NLS-1$
    protected static final String DATATYPE_NAME = "datatype-name"; //$NON-NLS-1$
    protected static final String DEFAULT_VALUE = "default-value"; //$NON-NLS-1$
    protected static final String DESCRIPTION = "description"; //$NON-NLS-1$
    protected static final String DISTINCT_VALUES = "distinct-values"; //$NON-NLS-1$
    protected static final String FIXED_LENGTH = "fixed-length"; //$NON-NLS-1$
    protected static final String LENGTH = "length"; //$NON-NLS-1$
    protected static final String MAX_VALUE = "max-value"; //$NON-NLS-1$
    protected static final String MIN_VALUE = "min-value"; //$NON-NLS-1$
    protected static final String NAME_IN_SOURCE = "name-in-source"; //$NON-NLS-1$
    protected static final String NATIVE_TYPE = "native-type"; //$NON-NLS-1$
    protected static final String NULLABLE = "nullable"; //$NON-NLS-1$
    protected static final String NULL_VALUE_COUNT = "null-value-count"; //$NON-NLS-1$
    protected static final String PRECISION = "precision"; //$NON-NLS-1$
    protected static final String RADIX = "radix"; //$NON-NLS-1$
    protected static final String SCALE = "scale"; //$NON-NLS-1$
    protected static final String SEARCHABLE = "searchable"; //$NON-NLS-1$
    protected static final String SELECTABLE = "selectable"; //$NON-NLS-1$
    protected static final String SIGNED = "signed"; //$NON-NLS-1$
    protected static final String UPDATABLE = "updatable"; //$NON-NLS-1$
    protected static final String UUID = "uuid"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { AUTO_INCREMENTED, CASE_SENSITIVE,
                                                                                    CHAR_OCTET_LENGTH, COLLATION_NAME, CURRENCY,
                                                                                    DATATYPE_NAME, DEFAULT_VALUE, DESCRIPTION,
                                                                                    DISTINCT_VALUES, FIXED_LENGTH, LENGTH,
                                                                                    MAX_VALUE, MIN_VALUE, NAME_IN_SOURCE,
                                                                                    NATIVE_TYPE, NULLABLE, NULL_VALUE_COUNT,
                                                                                    PRECISION, RADIX, SCALE, SEARCHABLE,
                                                                                    SELECTABLE, SIGNED, UPDATABLE, UUID } );

    protected ColumnShellCommand( final String name,
                                  final WorkspaceStatus status ) {
        super( status, name );
    }

    protected Column getColumn() throws Exception {
        return new ColumnImpl( getTransaction(), getRepository(), getPath() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return ColumnImpl.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            // exception returns false
        }
        return false;
    }

    @Override
    protected String getMessage(Enum< ? > key, Object... parameters) {
        return Messages.getString(ColumnCommandMessages.RESOURCE_BUNDLE,key.toString(),parameters);
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#printHelp(int indent)
     */
    @Override
    public void printHelp( final int indent ) {
        print( indent, Messages.getString( ColumnCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".help" ) ); //$NON-NLS-1$
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#printUsage(int indent)
     */
    @Override
    public void printUsage( final int indent ) {
        print( indent, Messages.getString( ColumnCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".usage" ) ); //$NON-NLS-1$
    }

}
