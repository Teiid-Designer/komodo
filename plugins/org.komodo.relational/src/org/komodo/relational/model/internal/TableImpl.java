/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.model.AccessPattern;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Index;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.modeshape.jcr.JcrLexicon;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.Constraint;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateTable;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.SchemaElement;

/**
 * An implementation of a relational model table.
 */
public class TableImpl extends RelationalObjectImpl implements Table {

    private enum StandardOptions {
        ANNOTATION,
        CARDINALITY,
        MATERIALIZED,
        MATERIALIZED_TABLE,
        NAMEINSOURCE,
        UPDATABLE,
        UUID
    }

    /*

      - ddl:temporary (STRING) < 'GLOBAL', 'LOCAL'
      - ddl:onCommitValue (STRING) < 'DELETE ROWS', 'PRESERVE ROWS'
      - teiidddl:schemaElementType (string) = 'FOREIGN' mandatory autocreated < 'FOREIGN', 'VIRTUAL'
      - teiidddl:queryExpression (string)
    + * (ddl:columnDefinition) = ddl:columnDefinition sns
    // TODO + * (ddl:tableConstraint) = ddl:tableConstraint sns
    + * (ddl:statementOption) = ddl:statementOption sns
    + * (teiidddl:constraint) = teiidddl:constraint sns

     */

    /**
     * The resolver of a {@link Table}.
     */
    public static final TypeResolver RESOLVER = new TypeResolver() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final Repository repository,
                                   final KomodoObject kobject ) {
            try {
                ObjectImpl.validateType(transaction, repository, kobject, CreateTable.TABLE_STATEMENT);
                return true;
            } catch (final Exception e) {
                // not resolvable
            }

            return false;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Table resolve( final UnitOfWork transaction,
                              final Repository repository,
                              final KomodoObject kobject ) throws KException {
            return new TableImpl(transaction, repository, kobject.getAbsolutePath());
        }

    };

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
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
    public AccessPattern addAccessPattern( final UnitOfWork uow,
                                           final String accessPatternName ) throws KException {
        ArgCheck.isNotEmpty(accessPatternName, "accessPatternName"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-addAccessPattern", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("addAccessPattern: transaction = {0}, accessPatternName = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         accessPatternName);
        }

        try {
            final AccessPattern result = RelationalModelFactory.createAccessPattern(transaction,
                                                                                    getRepository(),
                                                                                    this,
                                                                                    accessPatternName);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#addColumn(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Column addColumn( final UnitOfWork uow,
                             final String columnName ) throws KException {
        ArgCheck.isNotEmpty(columnName, "columnName"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-addColumn", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("addColumn: transaction = {0}, columnName = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         columnName);
        }

        try {
            final Column result = RelationalModelFactory.createColumn(transaction, getRepository(), this, columnName);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#addForeignKey(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      org.komodo.relational.model.Table)
     */
    @Override
    public ForeignKey addForeignKey( final UnitOfWork uow,
                                     final String foreignKeyName,
                                     final Table referencedTable ) throws KException {
        ArgCheck.isNotEmpty(foreignKeyName, "foreignKeyName"); //$NON-NLS-1$
        ArgCheck.isNotNull(referencedTable, "referencedTable"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-addForeignKey", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("addForeignKey: transaction = {0}, foreignKeyName = {1}, referencedTable = {2}", //$NON-NLS-1$
                         transaction.getName(),
                         foreignKeyName,
                         referencedTable);
        }

        try {
            final KomodoObject child = addChild(transaction, foreignKeyName, null);
            child.addDescriptor(transaction, Constraint.FOREIGN_KEY_CONSTRAINT);

            final String tableId = referencedTable.getProperty(transaction, JcrLexicon.UUID.getString()).getStringValue(transaction);
            child.setProperty(transaction, Constraint.TABLE_REFERENCE, tableId);

            final ForeignKey result = new ForeignKeyImpl(transaction, getRepository(), child.getAbsolutePath());

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#addIndex(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Index addIndex( final UnitOfWork uow,
                           final String indexName ) throws KException {
        ArgCheck.isNotEmpty(indexName, "indexName"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-addIndex", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("addIndex: transaction = {0}, indexName = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         indexName);
        }

        try {
            final Index result = RelationalModelFactory.createIndex(transaction, getRepository(), this, indexName);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#addUniqueConstraint(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public UniqueConstraint addUniqueConstraint( final UnitOfWork uow,
                                                 final String constraintName ) throws KException {
        ArgCheck.isNotEmpty(constraintName, "constraintName"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-addUniqueConstraint", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("addUniqueConstraint: transaction = {0}, constraintName = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         constraintName);
        }

        try {
            final UniqueConstraint result = RelationalModelFactory.createUniqueConstraint(transaction,
                                                                                          getRepository(),
                                                                                          this,
                                                                                          constraintName);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#getAccessPatterns(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public AccessPattern[] getAccessPatterns( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-getAccessPatterns", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("getAccessPatterns: transaction = {0}", transaction.getName()); //$NON-NLS-1$
        }

        try {
            final List< AccessPattern > result = new ArrayList< AccessPattern >();

            for (final KomodoObject kobject : super.getChildrenOfType(transaction, Constraint.TABLE_ELEMENT)) {
                final Property prop = kobject.getProperty(transaction, Constraint.TYPE);

                if (AccessPattern.CONSTRAINT_TYPE.toString().equals(prop.getStringValue(transaction))) {
                    final AccessPattern constraint = new AccessPatternImpl(transaction,
                                                                           getRepository(),
                                                                           kobject.getAbsolutePath());

                    if (LOGGER.isDebugEnabled()) {
                        LOGGER.debug("getAccessPatterns: transaction = {0}, found access pattern = {1}", //$NON-NLS-1$
                                     transaction.getName(),
                                     kobject.getAbsolutePath());
                    }

                    result.add(constraint);
                }
            }

            if (uow == null) {
                transaction.commit();
            }

            if (result.isEmpty()) {
                return AccessPattern.NO_ACCESS_PATTERNS;
            }

            return result.toArray(new AccessPattern[result.size()]);
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#getCardinality(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public int getCardinality( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.CARDINALITY.toString());

        if (option == null) {
            return Table.DEFAULT_CARDINALITY;
        }

        return Integer.parseInt(option.getOption(transaction));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoObject[] getChildren( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = getRepository().createTransaction("tableimpl-getChildren", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final AccessPattern[] accessPatterns = getAccessPatterns(transaction);
            final Column[] columns = getColumns(transaction);
            final ForeignKey[] foreignKeys = getForeignKeys(transaction);
            final Index[] indexes = getIndexes(transaction);
            final UniqueConstraint[] uniqueConstraints = getUniqueConstraints(transaction);

            final int size = accessPatterns.length + columns.length + foreignKeys.length + indexes.length
                             + uniqueConstraints.length;
            final KomodoObject[] result = new KomodoObject[size];
            System.arraycopy(accessPatterns, 0, result, 0, accessPatterns.length);
            System.arraycopy(columns, 0, result, accessPatterns.length, columns.length);
            System.arraycopy(foreignKeys, 0, result, accessPatterns.length + columns.length, foreignKeys.length);
            System.arraycopy(indexes, 0, result, accessPatterns.length + columns.length + foreignKeys.length, indexes.length);
            System.arraycopy(uniqueConstraints, 0, result, accessPatterns.length + columns.length + foreignKeys.length
                                                           + indexes.length, uniqueConstraints.length);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildrenOfType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public KomodoObject[] getChildrenOfType( final UnitOfWork uow,
                                             final String type ) throws KException {
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = getRepository().createTransaction("tableimpl-getChildrenOfType", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            KomodoObject[] result = KomodoObject.EMPTY_ARRAY;

            if (CreateTable.TABLE_ELEMENT.equals(type)) {
                result = getColumns(transaction);
            } else if (Constraint.TABLE_ELEMENT.equals(type)) {
                final AccessPattern[] accessPatterns = getAccessPatterns(transaction);
                final ForeignKey[] foreignKeys = getForeignKeys(transaction);
                final Index[] indexes = getIndexes(transaction);
                final UniqueConstraint[] uniqueConstraints = getUniqueConstraints(transaction);

                final int size = accessPatterns.length + foreignKeys.length + indexes.length + uniqueConstraints.length;
                final KomodoObject[] temp = new KomodoObject[size];
                System.arraycopy(accessPatterns, 0, temp, 0, accessPatterns.length);
                System.arraycopy(foreignKeys, 0, temp, accessPatterns.length, foreignKeys.length);
                System.arraycopy(indexes, 0, temp, accessPatterns.length + foreignKeys.length, indexes.length);
                System.arraycopy(uniqueConstraints,
                                 0,
                                 temp,
                                 accessPatterns.length + foreignKeys.length + indexes.length,
                                 uniqueConstraints.length);
                result = temp;
            }

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#getColumns(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Column[] getColumns( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = getRepository().createTransaction("tableimpl-getColumns", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("getColumns: transaction = {0}", transaction.getName()); //$NON-NLS-1$
        }

        try {
            final List< Column > result = new ArrayList< Column >();

            for (final KomodoObject kobject : super.getChildrenOfType(transaction, CreateTable.TABLE_ELEMENT)) {
                final Column column = new ColumnImpl(transaction, getRepository(), kobject.getAbsolutePath());

                if (LOGGER.isDebugEnabled()) {
                    LOGGER.debug("getColumns: transaction = {0}, found column = {1}", //$NON-NLS-1$
                                 transaction.getName(),
                                 kobject.getAbsolutePath());
                }

                result.add(column);
            }

            if (uow == null) {
                transaction.commit();
            }

            if (result.isEmpty()) {
                return Column.NO_COLUMNS;
            }

            return result.toArray(new Column[result.size()]);
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#getCustomOptions(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public StatementOption[] getCustomOptions( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-getCustomOptions", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            StatementOption[] result = getStatementOptions(transaction);

            if (result.length != 0) {
                final List< StatementOption > temp = new ArrayList<>(result.length);

                for (final StatementOption option : result) {
                    if (StandardOptions.valueOf(option.getName(transaction)) == null) {
                        temp.add(option);
                    }
                }

                result = temp.toArray(new StatementOption[temp.size()]);
            }

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#getDescription(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDescription( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.ANNOTATION.toString());

        if (option == null) {
            return null;
        }

        return option.getOption(transaction);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#getForeignKeys(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public ForeignKey[] getForeignKeys( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-getForeignKeys", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("getForeignKeys: transaction = {0}", transaction.getName()); //$NON-NLS-1$
        }

        try {
            final List< ForeignKey > result = new ArrayList< ForeignKey >();

            for (final KomodoObject kobject : super.getChildrenOfType(transaction, Constraint.FOREIGN_KEY_CONSTRAINT)) {
                final ForeignKey constraint = new ForeignKeyImpl(transaction, getRepository(), kobject.getAbsolutePath());

                if (LOGGER.isDebugEnabled()) {
                    LOGGER.debug("getForeignKeys: transaction = {0}, found foreign key constraint = {1}", //$NON-NLS-1$
                                 transaction.getName(),
                                 kobject.getAbsolutePath());
                }

                result.add(constraint);
            }

            if (uow == null) {
                transaction.commit();
            }

            if (result.isEmpty()) {
                return ForeignKey.NO_FOREIGN_KEYS;
            }

            return result.toArray(new ForeignKey[result.size()]);
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#getIndexes(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Index[] getIndexes( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-getIndexes", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("getIndexes: transaction = {0}", transaction.getName()); //$NON-NLS-1$
        }

        try {
            final List< Index > result = new ArrayList< Index >();

            for (final KomodoObject kobject : super.getChildrenOfType(transaction, Constraint.INDEX_CONSTRAINT)) {
                final Index constraint = new IndexImpl(transaction, getRepository(), kobject.getAbsolutePath());

                if (LOGGER.isDebugEnabled()) {
                    LOGGER.debug("getIndexes: transaction = {0}, found index constraint = {1}", //$NON-NLS-1$
                                 transaction.getName(),
                                 kobject.getAbsolutePath());
                }

                result.add(constraint);
            }

            if (uow == null) {
                transaction.commit();
            }

            if (result.isEmpty()) {
                return Index.NO_INDEXES;
            }

            return result.toArray(new Index[result.size()]);
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#getMaterializedTable(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getMaterializedTable( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.MATERIALIZED_TABLE.toString());

        if (option == null) {
            return null;
        }

        return option.getOption(transaction);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#getNameInSource(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getNameInSource( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.NAMEINSOURCE.toString());

        if (option == null) {
            return null;
        }

        return option.getOption(transaction);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#getOnCommitValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public OnCommit getOnCommitValue( final UnitOfWork uow ) throws KException {
        final String value = getObjectProperty(uow, Property.ValueType.STRING, "getOnCommitValue", //$NON-NLS-1$
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
    public PrimaryKey getPrimaryKey( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-getPrimaryKey", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("getPrimaryKey: transaction = {0}", transaction.getName()); //$NON-NLS-1$
        }

        try {
            PrimaryKey result = null;

            for (final KomodoObject kobject : super.getChildrenOfType(transaction, Constraint.TABLE_ELEMENT)) {
                final Property prop = kobject.getProperty(transaction, Constraint.TYPE);

                if (PrimaryKey.CONSTRAINT_TYPE.toString().equals(prop.getStringValue(transaction))) {
                    result = new PrimaryKeyImpl(transaction, getRepository(), kobject.getAbsolutePath());

                    if (LOGGER.isDebugEnabled()) {
                        LOGGER.debug("getPrimaryKey: transaction = {0}, found primary key = {1}", //$NON-NLS-1$
                                     transaction.getName(),
                                     kobject.getAbsolutePath());
                    }

                    break;
                }
            }

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#getQueryExpression(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getQueryExpression( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, Property.ValueType.STRING, "getQueryExpression", CreateTable.QUERY_EXPRESSION); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.SchemaElement#getSchemaElementType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public SchemaElementType getSchemaElementType( final UnitOfWork uow ) throws KException {
        final String value = getObjectProperty(uow, Property.ValueType.STRING, "getSchemaElementType", //$NON-NLS-1$
                                               SchemaElement.TYPE);

        if (StringUtils.isBlank(value)) {
            return null;
        }

        return SchemaElementType.fromValue(value);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#getStatementOptions(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public StatementOption[] getStatementOptions( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-getStatementOptions", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("getStatementOptions: transaction = {0}", transaction.getName()); //$NON-NLS-1$
        }

        try {
            final List< StatementOption > result = new ArrayList< StatementOption >();

            for (final KomodoObject kobject : super.getChildrenOfType(transaction, StandardDdlLexicon.TYPE_STATEMENT_OPTION)) {
                final StatementOption option = new StatementOptionImpl(transaction, getRepository(), kobject.getAbsolutePath());

                if (LOGGER.isDebugEnabled()) {
                    LOGGER.debug("getStatementOptions: transaction = {0}, found statement option = {1}", //$NON-NLS-1$
                                 transaction.getName(),
                                 kobject.getAbsolutePath());
                }

                result.add(option);
            }

            if (uow == null) {
                transaction.commit();
            }

            if (result.isEmpty()) {
                return StatementOption.NO_OPTIONS;
            }

            return result.toArray(new StatementOption[result.size()]);
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#getTemporaryTableType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public TemporaryType getTemporaryTableType( final UnitOfWork uow ) throws KException {
        final String value = getObjectProperty(uow, Property.ValueType.STRING, "getTemporaryTableType", //$NON-NLS-1$
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
     * @see org.komodo.relational.model.Table#getUniqueConstraints(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public UniqueConstraint[] getUniqueConstraints( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-getUniqueContraints", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("getUniqueContraints: transaction = {0}", transaction.getName()); //$NON-NLS-1$
        }

        try {
            final List< UniqueConstraint > result = new ArrayList< UniqueConstraint >();

            for (final KomodoObject kobject : super.getChildrenOfType(transaction, Constraint.TABLE_ELEMENT)) {
                final Property prop = kobject.getProperty(transaction, Constraint.TYPE);

                if (UniqueConstraint.CONSTRAINT_TYPE.toString().equals(prop.getStringValue(transaction))) {
                    final UniqueConstraint constraint = new UniqueConstraintImpl(transaction,
                                                                                 getRepository(),
                                                                                 kobject.getAbsolutePath());

                    if (LOGGER.isDebugEnabled()) {
                        LOGGER.debug("getUniqueContraints: transaction = {0}, found unique constraint = {1}", //$NON-NLS-1$
                                     transaction.getName(),
                                     kobject.getAbsolutePath());
                    }

                    result.add(constraint);
                }
            }

            if (uow == null) {
                transaction.commit();
            }

            if (result.isEmpty()) {
                return UniqueConstraint.NO_UNIQUE_CONSTRAINTS;
            }

            return result.toArray(new UniqueConstraint[result.size()]);
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#isMaterialized(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isMaterialized( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.MATERIALIZED.toString());

        if (option == null) {
            return Table.DEFAULT_MATERIALIZED;
        }

        return Boolean.parseBoolean(option.getOption(transaction));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#isUpdatable(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isUpdatable( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.UPDATABLE.toString());

        if (option == null) {
            return Table.DEFAULT_UPDATABLE;
        }

        return Boolean.parseBoolean(option.getOption(transaction));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#removeAccessPattern(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void removeAccessPattern( final UnitOfWork uow,
                                     final String accessPatternToRemove ) throws KException {
        ArgCheck.isNotEmpty(accessPatternToRemove, "accessPatternToRemove"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-removeAccessPattern", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removeAccessPattern: transaction = {0}, accessPatternToRemove = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         accessPatternToRemove);
        }

        boolean found = false;

        try {
            final AccessPattern[] accessPatterns = getAccessPatterns(transaction);

            if (accessPatterns.length != 0) {
                for (final AccessPattern accessPattern : accessPatterns) {
                    if (accessPatternToRemove.equals(accessPattern.getName(transaction))) {
                        removeChild(transaction, accessPatternToRemove);
                        found = true;
                        break;
                    }
                }
            }

            if (!found) {
                throw new KException(Messages.getString(Relational.CONSTRAINT_NOT_FOUND_TO_REMOVE,
                                                        accessPatternToRemove,
                                                        AccessPattern.CONSTRAINT_TYPE.toString()));
            }

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#removeColumn(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeColumn( final UnitOfWork uow,
                              final String columnToRemove ) throws KException {
        ArgCheck.isNotEmpty(columnToRemove, "columnToRemove"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-removeColumn", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removeColumn: transaction = {0}, columnToRemove = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         columnToRemove);
        }

        boolean found = false;

        try {
            final Column[] columns = getColumns(transaction);

            if (columns.length != 0) {
                for (final Column column : columns) {
                    if (columnToRemove.equals(column.getName(transaction))) {
                        removeChild(transaction, columnToRemove);
                        found = true;
                        break;
                    }
                }
            }

            if (!found) {
                throw new KException(Messages.getString(Relational.COLUMN_NOT_FOUND_TO_REMOVE, columnToRemove));
            }

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#removeForeignKey(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeForeignKey( final UnitOfWork uow,
                                  final String foreignKeyToRemove ) throws KException {
        ArgCheck.isNotEmpty(foreignKeyToRemove, "foreignKeyToRemove"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-removeForeignKey", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removeForeignKey: transaction = {0}, foreignKeyToRemove = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         foreignKeyToRemove);
        }

        boolean found = false;

        try {
            final ForeignKey[] foreignKeys = getForeignKeys(transaction);

            if (foreignKeys.length != 0) {
                for (final ForeignKey foreignKey : foreignKeys) {
                    if (foreignKeyToRemove.equals(foreignKey.getName(transaction))) {
                        removeChild(transaction, foreignKeyToRemove);
                        found = true;
                        break;
                    }
                }
            }

            if (!found) {
                throw new KException(Messages.getString(Relational.CONSTRAINT_NOT_FOUND_TO_REMOVE,
                                                        foreignKeyToRemove,
                                                        ForeignKey.CONSTRAINT_TYPE.toString()));
            }

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#removeIndex(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeIndex( final UnitOfWork uow,
                             final String indexToRemove ) throws KException {
        ArgCheck.isNotEmpty(indexToRemove, "indexToRemove"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-removeIndex", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removeIndex: transaction = {0}, indexToRemove = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         indexToRemove);
        }

        boolean found = false;

        try {
            final Index[] indexes = getIndexes(transaction);

            if (indexes.length != 0) {
                for (final Index index : indexes) {
                    if (indexToRemove.equals(index.getName(transaction))) {
                        removeChild(transaction, indexToRemove);
                        found = true;
                        break;
                    }
                }
            }

            if (!found) {
                throw new KException(Messages.getString(Relational.CONSTRAINT_NOT_FOUND_TO_REMOVE,
                                                        indexToRemove,
                                                        Index.CONSTRAINT_TYPE.toString()));
            }

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#removePrimaryKey(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public void removePrimaryKey( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-removePrimaryKey", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removePrimaryKey: transaction = {0}", transaction.getName()); //$NON-NLS-1$
        }

        try {
            final PrimaryKey primaryKey = getPrimaryKey(transaction);

            if (primaryKey == null) {
                throw new KException(Messages.getString(Relational.CONSTRAINT_NOT_FOUND_TO_REMOVE,
                                                        PrimaryKey.CONSTRAINT_TYPE.toString(),
                                                        PrimaryKey.CONSTRAINT_TYPE.toString()));
            }

            removeChild(transaction, primaryKey.getName(transaction));

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#removeStatementOption(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void removeStatementOption( final UnitOfWork uow,
                                       final String optionToRemove ) throws KException {
        ArgCheck.isNotEmpty(optionToRemove, "optionToRemove"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-removeStatementOption", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removeStatementOption: transaction = {0}, optionToRemove = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         optionToRemove);
        }

        boolean found = false;

        try {
            final StatementOption[] options = getStatementOptions(transaction);

            if (options.length != 0) {
                for (final StatementOption option : options) {
                    if (optionToRemove.equals(option.getName(transaction))) {
                        removeChild(transaction, optionToRemove);
                        found = true;
                        break;
                    }
                }
            }

            if (!found) {
                throw new KException(Messages.getString(Relational.STATEMENT_OPTION_NOT_FOUND_TO_REMOVE, optionToRemove));
            }

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#removeUniqueConstraint(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void removeUniqueConstraint( final UnitOfWork uow,
                                        final String constraintToRemove ) throws KException {
        ArgCheck.isNotEmpty(constraintToRemove, "constraintToRemove"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-removeUniqueConstraint", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removeUniqueConstraint: transaction = {0}, constraintToRemove = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         constraintToRemove);
        }

        boolean found = false;

        try {
            final UniqueConstraint[] uniqueConstraints = getUniqueConstraints(transaction);

            if (uniqueConstraints.length != 0) {
                for (final UniqueConstraint uniqueConstraint : uniqueConstraints) {
                    if (constraintToRemove.equals(uniqueConstraint.getName(transaction))) {
                        removeChild(transaction, constraintToRemove);
                        found = true;
                        break;
                    }
                }
            }

            if (!found) {
                throw new KException(Messages.getString(Relational.CONSTRAINT_NOT_FOUND_TO_REMOVE,
                                                        constraintToRemove,
                                                        UniqueConstraint.CONSTRAINT_TYPE.toString()));
            }

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setCardinality(org.komodo.spi.repository.Repository.UnitOfWork, int)
     */
    @Override
    public void setCardinality( final UnitOfWork transaction,
                                final int newCardinality ) throws KException {
        setStatementOption(transaction, StandardOptions.CARDINALITY.toString(), Integer.toString(newCardinality));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setDescription(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setDescription( final UnitOfWork transaction,
                                final String newDescription ) throws KException {
        setStatementOption(transaction, StandardOptions.ANNOTATION.toString(), newDescription);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setMaterialized(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setMaterialized( final UnitOfWork transaction,
                                 final boolean newMaterialized ) throws KException {
        setStatementOption(transaction, StandardOptions.MATERIALIZED.toString(), Boolean.toString(newMaterialized));
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
        setStatementOption(transaction, StandardOptions.MATERIALIZED_TABLE.toString(), newMaterializedTable);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setNameInSource(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setNameInSource( final UnitOfWork transaction,
                                 final String newNameInSource ) throws KException {
        setStatementOption(transaction, StandardOptions.NAMEINSOURCE.toString(), newNameInSource);
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
        final String newValue = (newOnCommit == null) ? null : newOnCommit.toString();
        setObjectProperty(uow, "setOnCommitValue", StandardDdlLexicon.ON_COMMIT_VALUE, newValue); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setPrimaryKey(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public PrimaryKey setPrimaryKey( final UnitOfWork uow,
                                     final String newPrimaryKeyName ) throws KException {
        ArgCheck.isNotEmpty(newPrimaryKeyName, "newPrimaryKeyName"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-setPrimaryKey", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setPrimaryKey: transaction = {0}, newPrimaryKeyName = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         newPrimaryKeyName);
        }

        try {
            // delete existing primary key
            final PrimaryKey primaryKey = getPrimaryKey(transaction);

            if (primaryKey != null) {
                removeChild(transaction, primaryKey.getName(transaction));
            }

            final PrimaryKey result = RelationalModelFactory.createPrimaryKey(transaction,
                                                                              getRepository(),
                                                                              this,
                                                                              newPrimaryKeyName);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
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
        final String newValue = ((newSchemaElementType == null) ? SchemaElementType.DEFAULT_VALUE.toString() : newSchemaElementType.toString());
        setObjectProperty(uow, "setSchemaElementType", SchemaElement.TYPE, newValue); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#setStatementOption(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String)
     */
    @Override
    public StatementOption setStatementOption( final UnitOfWork uow,
                                               final String optionName,
                                               final String optionValue ) throws KException {
        ArgCheck.isNotEmpty(optionName, "optionName"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-setStatementOption", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setStatementOption: transaction = {0}, optionName = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         optionName);
        }

        try {
            StatementOption result = null;

            if (StringUtils.isBlank(optionValue)) {
                removeStatementOption(transaction, optionName);
            } else {
                result = Utils.getOption(transaction, this, optionName);

                if (result == null) {
                    result = RelationalModelFactory.createStatementOption(transaction,
                                                                          getRepository(),
                                                                          this,
                                                                          optionName,
                                                                          optionValue);
                } else {
                    result.setOption(transaction, optionValue);
                }
            }

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
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
        final String newValue = ((newTempType == null) ? null : newTempType.toString());
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
        setStatementOption(transaction, StandardOptions.UPDATABLE.toString(), Boolean.toString(newUpdatable));
    }

}
