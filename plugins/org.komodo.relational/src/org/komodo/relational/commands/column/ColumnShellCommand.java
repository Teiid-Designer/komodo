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

    protected static final String AUTO_INCREMENTED = "autoIncrement"; //$NON-NLS-1$
    protected static final String CASE_SENSITIVE = "CASE_SENSITIVE"; //$NON-NLS-1$
    protected static final String CHAR_OCTET_LENGTH = "CHAR_OCTET_LENGTH"; //$NON-NLS-1$
    protected static final String COLLATION_NAME = "collationName"; //$NON-NLS-1$
    protected static final String CURRENCY = "CURRENCY"; //$NON-NLS-1$
    protected static final String DATATYPE_NAME = "datatypeName"; //$NON-NLS-1$
    protected static final String DEFAULT_VALUE = "defaultValue"; //$NON-NLS-1$
    protected static final String DESCRIPTION = "ANNOTATION"; //$NON-NLS-1$
    protected static final String DISTINCT_VALUES = "DISTINCT_VALUES"; //$NON-NLS-1$
    protected static final String FIXED_LENGTH = "FIXED_LENGTH"; //$NON-NLS-1$
    protected static final String LENGTH = "datatypeLength"; //$NON-NLS-1$
    protected static final String MAX_VALUE = "MAX_VALUE"; //$NON-NLS-1$
    protected static final String MIN_VALUE = "MIN_VALUE"; //$NON-NLS-1$
    protected static final String NAME_IN_SOURCE = "NAMEINSOURCE"; //$NON-NLS-1$
    protected static final String NATIVE_TYPE = "NATIVE_TYPE"; //$NON-NLS-1$
    protected static final String NULLABLE = "nullable"; //$NON-NLS-1$
    protected static final String NULL_VALUE_COUNT = "NULL_VALUE_COUNT"; //$NON-NLS-1$
    protected static final String PRECISION = "datatypePrecision"; //$NON-NLS-1$
    protected static final String RADIX = "RADIX"; //$NON-NLS-1$
    protected static final String SCALE = "datatypeScale"; //$NON-NLS-1$
    protected static final String SEARCHABLE = "SEARCHABLE"; //$NON-NLS-1$
    protected static final String SELECTABLE = "SELECTABLE"; //$NON-NLS-1$
    protected static final String SIGNED = "SIGNED"; //$NON-NLS-1$
    protected static final String UPDATABLE = "UPDATABLE"; //$NON-NLS-1$
    protected static final String UUID = "UUID"; //$NON-NLS-1$

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

    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, Messages.getString( ColumnCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".help", getName() ) ); //$NON-NLS-1$
    }

    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, Messages.getString( ColumnCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".examples" ) ); //$NON-NLS-1$
    }

    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, Messages.getString( ColumnCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".usage" ) ); //$NON-NLS-1$
    }

}
