/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.relational.model.internal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import org.komodo.modeshape.visitor.DdlNodeVisitor;
import org.komodo.modeshape.visitor.DdlNodeVisitor.VisitorExclusions;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.AccessPattern;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Index;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.spi.KException;
import org.komodo.spi.constants.ExportConstants;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.DocumentType;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.version.TeiidVersionProvider;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.Constraint;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.CreateTable;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.SchemaElement;

/**
 * An implementation of a relational model table.
 */
public class TableImpl extends RelationalObjectImpl implements Table {

    /**
     * The allowed child types.
     */
    private static final KomodoType[] CHILD_TYPES = new KomodoType[] { AccessPattern.IDENTIFIER, Column.IDENTIFIER,
                                                                      ForeignKey.IDENTIFIER, Index.IDENTIFIER,
                                                                      PrimaryKey.IDENTIFIER, UniqueConstraint.IDENTIFIER };

    private enum StandardOption {

        ANNOTATION( null ),
        CARDINALITY( Long.toString( Table.DEFAULT_CARDINALITY ) ),
        MATERIALIZED( Boolean.toString( Table.DEFAULT_MATERIALIZED ) ),
        MATERIALIZED_TABLE( null ),
        NAMEINSOURCE( null ),
        UPDATABLE( Boolean.toString( Table.DEFAULT_UPDATABLE ) ),
        UUID( null );

        private static Map< String, String > _defaultValues = null;

        /**
         * @return an unmodifiable collection of the names and default values of all the standard options (never <code>null</code>
         *         or empty)
         */
        static Map< String, String > defaultValues() {
            if ( _defaultValues == null ) {
                final StandardOption[] options = values();
                final Map< String, String > temp = new HashMap< >();

                for ( final StandardOption option : options ) {
                    temp.put( option.name(), option.defaultValue );
                }

                _defaultValues = Collections.unmodifiableMap( temp );
            }

            return _defaultValues;
        }

        /**
         * @param name
         *        the name being checked (can be <code>null</code>)
         * @return <code>true</code> if the name is the name of a standard option
         */
        static boolean isValid( final String name ) {
            for ( final StandardOption option : values() ) {
                if ( option.name().equals( name ) ) {
                    return true;
                }
            }

            return false;
        }

        private final String defaultValue;

        private StandardOption( final String defaultValue ) {
            this.defaultValue = defaultValue;
        }

    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs or if node at specified path is not a table
     */
    public TableImpl( final UnitOfWork uow,
                      final Repository repository,
                      final String workspacePath ) throws KException {
        super(uow, repository, workspacePath);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#addAccessPattern(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public AccessPattern addAccessPattern( final UnitOfWork transaction,
                                           final String accessPatternName ) throws KException {
        return RelationalModelFactory.createAccessPattern( transaction, getRepository(), this, accessPatternName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#addColumn(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Column addColumn( final UnitOfWork transaction,
                             final String columnName ) throws KException {
        return RelationalModelFactory.createColumn( transaction, getRepository(), this, columnName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#addForeignKey(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      org.komodo.relational.model.Table)
     */
    @Override
    public ForeignKey addForeignKey( final UnitOfWork transaction,
                                     final String foreignKeyName,
                                     final Table referencedTable ) throws KException {
        return RelationalModelFactory.createForeignKey( transaction, getRepository(), this, foreignKeyName, referencedTable );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#addIndex(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Index addIndex( final UnitOfWork transaction,
                           final String indexName ) throws KException {
        return RelationalModelFactory.createIndex( transaction, getRepository(), this, indexName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#addUniqueConstraint(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public UniqueConstraint addUniqueConstraint( final UnitOfWork transaction,
                                                 final String constraintName ) throws KException {
        return RelationalModelFactory.createUniqueConstraint( transaction, getRepository(), this, constraintName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#getAccessPatterns(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public AccessPattern[] getAccessPatterns( final UnitOfWork transaction,
                                              final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< AccessPattern > result = new ArrayList< AccessPattern >();

        for ( final KomodoObject kobject : getChildrenOfType( transaction, Constraint.TABLE_ELEMENT, namePatterns ) ) {
            final Property prop = kobject.getRawProperty( transaction, Constraint.TYPE );

            if ( AccessPattern.CONSTRAINT_TYPE.toValue().equals( prop.getStringValue( transaction ) ) ) {
                final AccessPattern constraint = new AccessPatternImpl( transaction, getRepository(), kobject.getAbsolutePath() );
                result.add( constraint );
            }
        }

        if ( result.isEmpty() ) {
            return AccessPattern.NO_ACCESS_PATTERNS;
        }

        return result.toArray( new AccessPattern[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#getCardinality(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public long getCardinality( final UnitOfWork transaction ) throws KException {
        final String option = OptionContainerUtils.getOption( transaction, this, StandardOption.CARDINALITY.name() );

        if ( option == null ) {
            return Table.DEFAULT_CARDINALITY;
        }

        return Long.parseLong( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildren(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public KomodoObject[] getChildren( final UnitOfWork transaction,
                                       final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        // constraints are access patterns, primary key, and unique constraints
        final KomodoObject[] constraints = getChildrenOfType( transaction, Constraint.TABLE_ELEMENT, namePatterns );
        final Column[] columns = getColumns( transaction, namePatterns );
        final ForeignKey[] foreignKeys = getForeignKeys( transaction, namePatterns );
        final Index[] indexes = getIndexes( transaction, namePatterns );

        final int size = constraints.length + columns.length + foreignKeys.length + indexes.length;
        final KomodoObject[] result = new KomodoObject[ size ];
        System.arraycopy( constraints, 0, result, 0, constraints.length );
        System.arraycopy( columns, 0, result, constraints.length, columns.length );
        System.arraycopy( foreignKeys, 0, result, constraints.length + columns.length, foreignKeys.length );
        System.arraycopy( indexes, 0, result, constraints.length + columns.length + foreignKeys.length, indexes.length );

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildTypes()
     */
    @Override
    public KomodoType[] getChildTypes() {
        return CHILD_TYPES;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#getColumns(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public Column[] getColumns( final UnitOfWork transaction,
                                final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< Column > result = new ArrayList< Column >();

        for ( final KomodoObject kobject : getChildrenOfType( transaction, CreateTable.TABLE_ELEMENT, namePatterns ) ) {
            final Column column = new ColumnImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            result.add( column );
        }

        if ( result.isEmpty() ) {
            return Column.NO_COLUMNS;
        }

        return result.toArray( new Column[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#getCustomOptions(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public StatementOption[] getCustomOptions( final UnitOfWork transaction ) throws KException {
        return OptionContainerUtils.getCustomOptions( transaction, this );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#getDescription(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDescription( final UnitOfWork transaction ) throws KException {
        return OptionContainerUtils.getOption(transaction, this, StandardOption.ANNOTATION.name());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#getForeignKeys(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public ForeignKey[] getForeignKeys( final UnitOfWork transaction,
                                        final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< ForeignKey > result = new ArrayList< ForeignKey >();

        for ( final KomodoObject kobject : getChildrenOfType( transaction, Constraint.FOREIGN_KEY_CONSTRAINT, namePatterns ) ) {
            final ForeignKey constraint = new ForeignKeyImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            result.add( constraint );
        }

        if ( result.isEmpty() ) {
            return ForeignKey.NO_FOREIGN_KEYS;
        }

        return result.toArray( new ForeignKey[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#getIndexes(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public Index[] getIndexes( final UnitOfWork transaction,
                               final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< Index > result = new ArrayList< Index >();

        for ( final KomodoObject kobject : getChildrenOfType( transaction, Constraint.INDEX_CONSTRAINT, namePatterns ) ) {
            final Index constraint = new IndexImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            result.add( constraint );
        }

        if ( result.isEmpty() ) {
            return Index.NO_INDEXES;
        }

        return result.toArray( new Index[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#getMaterializedTable(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getMaterializedTable( final UnitOfWork transaction ) throws KException {
        return OptionContainerUtils.getOption(transaction, this, StandardOption.MATERIALIZED_TABLE.name());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#getNameInSource(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getNameInSource( final UnitOfWork transaction ) throws KException {
        return OptionContainerUtils.getOption(transaction, this, StandardOption.NAMEINSOURCE.name());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#getOnCommitValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public OnCommit getOnCommitValue( final UnitOfWork uow ) throws KException {
        final String value = getObjectProperty(uow, PropertyValueType.STRING, "getOnCommitValue", //$NON-NLS-1$
                                               StandardDdlLexicon.ON_COMMIT_VALUE);

        if (StringUtils.isBlank(value)) {
            return null;
        }

        return OnCommit.fromValue(value);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#getPrimaryKey(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public PrimaryKey getPrimaryKey( final UnitOfWork transaction ) throws KException {
        PrimaryKey result = null;

        for ( final KomodoObject kobject : getChildrenOfType( transaction, Constraint.TABLE_ELEMENT ) ) {
            final Property prop = kobject.getRawProperty( transaction, Constraint.TYPE );

            if ( PrimaryKey.CONSTRAINT_TYPE.toValue().equals( prop.getStringValue( transaction ) ) ) {
                result = new PrimaryKeyImpl( transaction, getRepository(), kobject.getAbsolutePath() );
                break;
            }
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getPrimaryType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Descriptor getPrimaryType( final UnitOfWork transaction ) throws KException {
        return OptionContainerUtils.createPrimaryType(transaction, this, super.getPrimaryType( transaction ));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getProperty(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public Property getProperty( final UnitOfWork transaction,
                                 final String name ) throws KException {
        return OptionContainerUtils.getProperty( transaction, this, name, super.getProperty( transaction, name ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getPropertyDescriptor(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public PropertyDescriptor getPropertyDescriptor( final UnitOfWork transaction,
                                                     final String propName ) throws KException {
        return OptionContainerUtils.getPropertyDescriptor( transaction,
                                                           this,
                                                           propName,
                                                           super.getPropertyDescriptor( transaction, propName ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getPropertyNames(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String[] getPropertyNames( final UnitOfWork transaction ) throws KException {
        return OptionContainerUtils.getPropertyNames( transaction, this, super.getPropertyNames( transaction ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#getQueryExpression(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getQueryExpression( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.STRING, "getQueryExpression", CreateTable.QUERY_EXPRESSION); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.SchemaElement#getSchemaElementType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public SchemaElementType getSchemaElementType( final UnitOfWork uow ) throws KException {
        final String value = getObjectProperty(uow, PropertyValueType.STRING, "getSchemaElementType", //$NON-NLS-1$
                                               SchemaElement.TYPE);

        if (StringUtils.isBlank(value)) {
            return null;
        }

        return SchemaElementType.fromValue(value);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#getStandardOptions()
     */
    @Override
    public Map< String, String > getStandardOptions() {
        return StandardOption.defaultValues();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#getStatementOptionNames(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String[] getStatementOptionNames( final UnitOfWork transaction ) throws KException {
        return OptionContainerUtils.getOptionNames( transaction, this );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#getStatementOptions(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public StatementOption[] getStatementOptions( final UnitOfWork transaction ) throws KException {
        return OptionContainerUtils.getOptions( transaction, this );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#getTemporaryTableType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public TemporaryType getTemporaryTableType( final UnitOfWork uow ) throws KException {
        final String value = getObjectProperty(uow, PropertyValueType.STRING, "getTemporaryTableType", //$NON-NLS-1$
                                               StandardDdlLexicon.TEMPORARY);

        if (StringUtils.isBlank(value)) {
            return null;
        }

        return TemporaryType.fromValue(value);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getTypeId()
     */
    @Override
    public int getTypeId() {
        return TYPE_ID;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getTypeIdentifier(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoType getTypeIdentifier( final UnitOfWork uow ) {
        return Table.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#getUniqueConstraints(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public UniqueConstraint[] getUniqueConstraints( final UnitOfWork transaction,
                                                    final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< UniqueConstraint > result = new ArrayList< UniqueConstraint >();

        for ( final KomodoObject kobject : getChildrenOfType( transaction, Constraint.TABLE_ELEMENT, namePatterns ) ) {
            final Property prop = kobject.getRawProperty( transaction, Constraint.TYPE );

            if ( UniqueConstraint.CONSTRAINT_TYPE.toValue().equals( prop.getStringValue( transaction ) ) ) {
                final UniqueConstraint constraint = new UniqueConstraintImpl( transaction,
                                                                              getRepository(),
                                                                              kobject.getAbsolutePath() );
                result.add( constraint );
            }
        }

        if ( result.isEmpty() ) {
            return UniqueConstraint.NO_UNIQUE_CONSTRAINTS;
        }

        return result.toArray( new UniqueConstraint[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#getUuid(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getUuid( final UnitOfWork transaction ) throws KException {
        return OptionContainerUtils.getOption( transaction, this, StandardOption.UUID.name() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Model getParent( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject parent = super.getParent( transaction );
        final Model result = Model.RESOLVER.resolve( transaction, parent );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#hasProperties(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean hasProperties( final UnitOfWork transaction ) throws KException {
        return OptionContainerUtils.hasProperties( transaction, this, super.hasProperties( transaction ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasProperty(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public boolean hasProperty( final UnitOfWork transaction,
                                final String name ) throws KException {
        return OptionContainerUtils.hasProperty( transaction, this, name, super.hasProperty( transaction, name ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#isCustomOption(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public boolean isCustomOption( final UnitOfWork transaction,
                                   final String name ) throws KException {
        return OptionContainerUtils.hasCustomOption( transaction, this, name );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#isMaterialized(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isMaterialized( final UnitOfWork transaction ) throws KException {
        final String option = OptionContainerUtils.getOption( transaction, this, StandardOption.MATERIALIZED.name() );

        if ( option == null ) {
            return Table.DEFAULT_MATERIALIZED;
        }

        return Boolean.parseBoolean( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#isStandardOption(java.lang.String)
     */
    @Override
    public boolean isStandardOption( final String name ) {
        return StandardOption.isValid( name );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#isUpdatable(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isUpdatable( final UnitOfWork transaction ) throws KException {
        final String option = OptionContainerUtils.getOption( transaction, this, StandardOption.UPDATABLE.name() );

        if ( option == null ) {
            return Table.DEFAULT_UPDATABLE;
        }

        return Boolean.parseBoolean( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#removeAccessPattern(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void removeAccessPattern( final UnitOfWork transaction,
                                     final String accessPatternToRemove ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( accessPatternToRemove, "accessPatternToRemove" ); //$NON-NLS-1$

        final AccessPattern[] accessPatterns = getAccessPatterns( transaction, accessPatternToRemove );

        if ( accessPatterns.length == 0 ) {
            throw new KException( Messages.getString( Relational.CONSTRAINT_NOT_FOUND_TO_REMOVE,
                                                      accessPatternToRemove,
                                                      AccessPattern.CONSTRAINT_TYPE.toString() ) );
        }

        // remove first occurrence
        accessPatterns[ 0 ].remove( transaction );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#removeColumn(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeColumn( final UnitOfWork transaction,
                              final String columnToRemove ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( columnToRemove, "columnToRemove" ); //$NON-NLS-1$

        final Column[] columns = getColumns( transaction, columnToRemove );

        if ( columns.length == 0 ) {
            throw new KException( Messages.getString( Relational.COLUMN_NOT_FOUND_TO_REMOVE, columnToRemove ) );
        }

        // remove first occurrence
        columns[ 0 ].remove( transaction );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#removeForeignKey(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeForeignKey( final UnitOfWork transaction,
                                  final String foreignKeyToRemove ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( foreignKeyToRemove, "foreignKeyToRemove" ); //$NON-NLS-1$

        final ForeignKey[] foreignKeys = getForeignKeys( transaction, foreignKeyToRemove );

        if ( foreignKeys.length == 0 ) {
            throw new KException( Messages.getString( Relational.CONSTRAINT_NOT_FOUND_TO_REMOVE,
                                                      foreignKeyToRemove,
                                                      ForeignKey.CONSTRAINT_TYPE.toString() ) );
        }

        // remove first occurrence
        foreignKeys[ 0 ].remove( transaction );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#removeIndex(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeIndex( final UnitOfWork transaction,
                             final String indexToRemove ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( indexToRemove, "indexToRemove" ); //$NON-NLS-1$

        final Index[] indexes = getIndexes( transaction, indexToRemove );

        if ( indexes.length == 0 ) {
            throw new KException( Messages.getString( Relational.CONSTRAINT_NOT_FOUND_TO_REMOVE,
                                                      indexToRemove,
                                                      Index.CONSTRAINT_TYPE.toString() ) );
        }

        // remove first occurrence
        indexes[ 0 ].remove( transaction );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#removePrimaryKey(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public void removePrimaryKey( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final PrimaryKey primaryKey = getPrimaryKey( transaction );

        if ( primaryKey == null ) {
            throw new KException( Messages.getString( Relational.CONSTRAINT_NOT_FOUND_TO_REMOVE,
                                                      PrimaryKey.CONSTRAINT_TYPE.toString(),
                                                      PrimaryKey.CONSTRAINT_TYPE.toString() ) );
        }

        primaryKey.remove( transaction );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#removeStatementOption(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void removeStatementOption( final UnitOfWork transaction,
                                       final String optionToRemove ) throws KException {
        OptionContainerUtils.removeOption( transaction, this, optionToRemove );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#removeUniqueConstraint(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void removeUniqueConstraint( final UnitOfWork transaction,
                                        final String constraintToRemove ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( constraintToRemove, "constraintToRemove" ); //$NON-NLS-1$

        final UniqueConstraint[] uniqueConstraints = getUniqueConstraints( transaction, constraintToRemove );

        if ( uniqueConstraints.length == 0 ) {
            throw new KException( Messages.getString( Relational.CONSTRAINT_NOT_FOUND_TO_REMOVE,
                                                      constraintToRemove,
                                                      UniqueConstraint.CONSTRAINT_TYPE.toString() ) );
        }

        // remove first occurrence
        uniqueConstraints[ 0 ].remove( transaction );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setCardinality(org.komodo.spi.repository.Repository.UnitOfWork, long)
     */
    @Override
    public void setCardinality( final UnitOfWork transaction,
                                final long newCardinality ) throws KException {
        setStatementOption(transaction, StandardOption.CARDINALITY.name(), Long.toString(newCardinality));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setDescription(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setDescription( final UnitOfWork transaction,
                                final String newDescription ) throws KException {
        setStatementOption(transaction, StandardOption.ANNOTATION.name(), newDescription);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setMaterialized(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setMaterialized( final UnitOfWork transaction,
                                 final boolean newMaterialized ) throws KException {
        setStatementOption(transaction, StandardOption.MATERIALIZED.name(), Boolean.toString(newMaterialized));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setMaterializedTable(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setMaterializedTable( final UnitOfWork transaction,
                                      final String newMaterializedTable ) throws KException {
        setStatementOption(transaction, StandardOption.MATERIALIZED_TABLE.name(), newMaterializedTable);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setNameInSource(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setNameInSource( final UnitOfWork transaction,
                                 final String newNameInSource ) throws KException {
        setStatementOption(transaction, StandardOption.NAMEINSOURCE.name(), newNameInSource);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setOnCommitValue(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.model.Table.OnCommit)
     */
    @Override
    public void setOnCommitValue( final UnitOfWork uow,
                                  final OnCommit newOnCommit ) throws KException {
        final String newValue = (newOnCommit == null) ? null : newOnCommit.toValue();
        setObjectProperty(uow, "setOnCommitValue", StandardDdlLexicon.ON_COMMIT_VALUE, newValue); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setPrimaryKey(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public PrimaryKey setPrimaryKey( final UnitOfWork transaction,
                                     final String newPrimaryKeyName ) throws KException {
        // delete existing primary key (don't call removePrimaryKey as it throws exception if one does not exist)
        final PrimaryKey primaryKey = getPrimaryKey( transaction );

        if ( primaryKey != null ) {
            primaryKey.remove( transaction );
        }

        final PrimaryKey result = RelationalModelFactory.createPrimaryKey( transaction, getRepository(), this, newPrimaryKeyName );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#setProperty(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.Object[])
     */
    @Override
    public void setProperty( final UnitOfWork transaction,
                             final String propertyName,
                             final Object... values ) throws KException {
        // if an option was not set then set a property
        if ( !OptionContainerUtils.setProperty( transaction, this, propertyName, values ) ) {
            super.setProperty( transaction, propertyName, values );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setQueryExpression(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setQueryExpression( final UnitOfWork uow,
                                    final String newQueryExpression ) throws KException {
        setObjectProperty(uow, "setQueryExpression", CreateTable.QUERY_EXPRESSION, newQueryExpression); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.SchemaElement#setSchemaElementType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.model.SchemaElement.SchemaElementType)
     */
    @Override
    public void setSchemaElementType( final UnitOfWork uow,
                                      final SchemaElementType newSchemaElementType ) throws KException {
        final String newValue = ((newSchemaElementType == null) ? SchemaElementType.DEFAULT_VALUE.name() : newSchemaElementType.name());
        setObjectProperty(uow, "setSchemaElementType", SchemaElement.TYPE, newValue); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#setStatementOption(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String)
     */
    @Override
    public StatementOption setStatementOption( final UnitOfWork transaction,
                                               final String optionName,
                                               final String optionValue ) throws KException {
        return OptionContainerUtils.setOption( transaction, this, optionName, optionValue );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setTemporaryTableType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.model.Table.TemporaryType)
     */
    @Override
    public void setTemporaryTableType( final UnitOfWork uow,
                                       final TemporaryType newTempType ) throws KException {
        final String newValue = ((newTempType == null) ? null : newTempType.name());
        setObjectProperty(uow, "setTemporaryTableType", StandardDdlLexicon.TEMPORARY, newValue); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setUpdatable(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setUpdatable( final UnitOfWork transaction,
                              final boolean newUpdatable ) throws KException {
        setStatementOption(transaction, StandardOption.UPDATABLE.name(), Boolean.toString(newUpdatable));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setUuid(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setUuid( final UnitOfWork transaction,
                         final String newUuid ) throws KException {
        setStatementOption( transaction, StandardOption.UUID.name(), newUuid );
    }

    private String exportDdl(UnitOfWork transaction, Properties exportProperties) throws Exception {
        List<VisitorExclusions> exclusions = new ArrayList<VisitorExclusions>();
        if( exportProperties != null && !exportProperties.isEmpty() ) {
            if(exportProperties.containsKey(ExportConstants.EXCLUDE_TABLE_CONSTRAINTS_KEY)) {
                exclusions.add(VisitorExclusions.EXCLUDE_TABLE_CONSTRAINTS);
            }
        }
        DdlNodeVisitor visitor = new DdlNodeVisitor(TeiidVersionProvider.getInstance().getTeiidVersion(), false, exclusions.toArray(new VisitorExclusions[0]));
        visitor.visit(node(transaction));

        String result = visitor.getDdl();
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Exportable#export(org.komodo.spi.repository.Repository.UnitOfWork, java.util.Properties)
     */
    @Override
    public byte[] export( final UnitOfWork transaction , Properties exportProperties) throws KException {
        ArgCheck.isNotNull(transaction);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("tableimpl-export: transaction = {0}", transaction.getName()); //$NON-NLS-1$
        }

        try {
            String result = exportDdl(transaction, exportProperties);

            if (LOGGER.isDebugEnabled()) {
                LOGGER.debug("TableImpl: transaction = {0}, xml = {1}", //$NON-NLS-1$
                             transaction.getName(),
                             result);
            }

            return result.getBytes();

        } catch (final Exception e) {
            throw handleError(e);
        }
    }

    @Override
    public DocumentType getDocumentType(UnitOfWork transaction) throws KException {
        return DocumentType.DDL;
    }
}
