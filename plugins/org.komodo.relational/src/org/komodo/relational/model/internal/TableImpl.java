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
import org.komodo.relational.RelationalConstants.OnCommit;
import org.komodo.relational.RelationalConstants.TemporaryType;
import org.komodo.relational.model.AccessPattern;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Index;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.UniqueConstraint;
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
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public TableImpl( final Repository repository,
                      final String workspacePath ) throws KException {
        super(repository, workspacePath);
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
            LOGGER.debug("addAccessPattern: transaction = '{0}', accessPatternName = '{1}'", //$NON-NLS-1$
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
            LOGGER.debug("addColumn: transaction = '{0}', columnName = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         columnName);
        }

        try {
            final Column result = RelationalModelFactory.createColumn(transaction, getRepository(), getAbsolutePath(), columnName);

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
            LOGGER.debug("addForeignKey: transaction = '{0}', foreignKeyName = '{1}', referencedTable = '{2}'", //$NON-NLS-1$
                         transaction.getName(),
                         foreignKeyName,
                         referencedTable);
        }

        try {
            final KomodoObject child = addChild(transaction, foreignKeyName, null);
            child.addDescriptor(transaction, Constraint.FOREIGN_KEY_CONSTRAINT);

            final String tableId = referencedTable.getProperty(transaction, JcrLexicon.UUID.getString()).getStringValue();
            child.setProperty(transaction, Constraint.TABLE_REFERENCE, tableId);

            final ForeignKey result = new ForeignKeyImpl(getRepository(), child.getAbsolutePath());

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
            LOGGER.debug("addIndex: transaction = '{0}', indexName = '{1}'", //$NON-NLS-1$
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
     * @see org.komodo.relational.model.Table#addStatementOption(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String)
     */
    @Override
    public StatementOption addStatementOption( final UnitOfWork uow,
                                               final String optionName,
                                               final String optionValue ) throws KException {
        ArgCheck.isNotEmpty(optionName, "optionName"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(optionValue, "optionValue"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-addStatementOption", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("addStatementOption: transaction = '{0}', optionName = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         optionName);
        }

        try {
            final StatementOption result = RelationalModelFactory.createStatementOption(transaction,
                                                                                        getRepository(),
                                                                                        getAbsolutePath(),
                                                                                        optionName,
                                                                                        optionValue);

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
            LOGGER.debug("addUniqueConstraint: transaction = '{0}', constraintName = '{1}'", //$NON-NLS-1$
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
            LOGGER.debug("getAccessPatterns: transaction = '{0}'", transaction.getName()); //$NON-NLS-1$
        }

        try {
            final List< AccessPattern > result = new ArrayList< AccessPattern >();

            for (final KomodoObject kobject : getChildrenOfType(transaction, Constraint.TABLE_ELEMENT)) {
                final Property prop = kobject.getProperty(transaction, Constraint.TYPE);

                if (AccessPattern.CONSTRAINT_TYPE.toString().equals(prop.getStringValue())) {
                    final AccessPattern constraint = new AccessPatternImpl(getRepository(), kobject.getAbsolutePath());

                    if (LOGGER.isDebugEnabled()) {
                        LOGGER.debug("getAccessPatterns: transaction = '{0}', found access pattern = '{1}'", //$NON-NLS-1$
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
     * @see org.komodo.relational.model.Table#getColumns(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Column[] getColumns( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-getColumns", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("getColumns: transaction = '{0}'", transaction.getName()); //$NON-NLS-1$
        }

        try {
            final List< Column > result = new ArrayList< Column >();

            for (final KomodoObject kobject : getChildrenOfType(transaction, CreateTable.TABLE_ELEMENT)) {
                final Column column = new ColumnImpl(getRepository(), kobject.getAbsolutePath());

                if (LOGGER.isDebugEnabled()) {
                    LOGGER.debug("getForeignKeys: transaction = '{0}', found column = '{1}'", //$NON-NLS-1$
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
            LOGGER.debug("getForeignKeys: transaction = '{0}'", transaction.getName()); //$NON-NLS-1$
        }

        try {
            final List< ForeignKey > result = new ArrayList< ForeignKey >();

            for (final KomodoObject kobject : getChildrenOfType(transaction, Constraint.FOREIGN_KEY_CONSTRAINT)) {
                final ForeignKey constraint = new ForeignKeyImpl(getRepository(), kobject.getAbsolutePath());

                if (LOGGER.isDebugEnabled()) {
                    LOGGER.debug("getForeignKeys: transaction = '{0}', found foreign key constraint = '{1}'", //$NON-NLS-1$
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
            LOGGER.debug("getIndexes: transaction = '{0}'", transaction.getName()); //$NON-NLS-1$
        }

        try {
            final List< Index > result = new ArrayList< Index >();

            for (final KomodoObject kobject : getChildrenOfType(transaction, Constraint.INDEX_CONSTRAINT)) {
                final Index constraint = new IndexImpl(getRepository(), kobject.getAbsolutePath());

                if (LOGGER.isDebugEnabled()) {
                    LOGGER.debug("getIndexes: transaction = '{0}', found index constraint = '{1}'", //$NON-NLS-1$
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
     * @see org.komodo.relational.model.Table#getOnCommitValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public OnCommit getOnCommitValue( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableImpl-getOnCommitValue", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            OnCommit result = null;
            final Property property = getProperty(transaction, StandardDdlLexicon.ON_COMMIT_VALUE);

            if (property != null) {
                final String value = property.getStringValue();
                result = OnCommit.fromValue(value);
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
            LOGGER.debug("getPrimaryKey: transaction = '{0}'", transaction.getName()); //$NON-NLS-1$
        }

        try {
            PrimaryKey result = null;

            for (final KomodoObject kobject : getChildrenOfType(transaction, Constraint.TABLE_ELEMENT)) {
                final Property prop = kobject.getProperty(transaction, Constraint.TYPE);

                if (PrimaryKey.CONSTRAINT_TYPE.toString().equals(prop.getStringValue())) {
                    result = new PrimaryKeyImpl(getRepository(), kobject.getAbsolutePath());

                    if (LOGGER.isDebugEnabled()) {
                        LOGGER.debug("getPrimaryKey: transaction = '{0}', found primary key = '{1}'", //$NON-NLS-1$
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
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-getColumns", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            String result = null;
            final Property property = getProperty(transaction, CreateTable.QUERY_EXPRESSION);

            if (property != null) {
                result = property.getStringValue();
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
     * @see org.komodo.relational.model.SchemaElement#getSchemaElementType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public SchemaElementType getSchemaElementType( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableImpl-getSchemaElementType", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            SchemaElementType result = SchemaElementType.DEFAULT_VALUE;
            final Property property = getProperty(transaction, SchemaElement.TYPE);

            if (property != null) {
                final String value = property.getStringValue();
                result = SchemaElementType.fromValue(value);
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
     * @see org.komodo.relational.model.Table#getStatementOptions(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public StatementOption[] getStatementOptions( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-getStatementOptions", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("getStatementOptions: transaction = '{0}'", transaction.getName()); //$NON-NLS-1$
        }

        try {
            final List< StatementOption > result = new ArrayList< StatementOption >();

            for (final KomodoObject kobject : getChildrenOfType(transaction, StandardDdlLexicon.TYPE_STATEMENT_OPTION)) {
                final StatementOption option = new StatementOptionImpl(getRepository(), kobject.getAbsolutePath());

                if (LOGGER.isDebugEnabled()) {
                    LOGGER.debug("getStatementOptions: transaction = '{0}', found statement option = '{1}'", //$NON-NLS-1$
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
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-getTemporyTableType", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            TemporaryType result = null;
            final Property property = getProperty(transaction, StandardDdlLexicon.TEMPORARY);

            if (property != null) {
                final String value = property.getStringValue();
                result = TemporaryType.fromValue(value);
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
            LOGGER.debug("getUniqueContraints: transaction = '{0}'", transaction.getName()); //$NON-NLS-1$
        }

        try {
            final List< UniqueConstraint > result = new ArrayList< UniqueConstraint >();

            for (final KomodoObject kobject : getChildrenOfType(transaction, Constraint.TABLE_ELEMENT)) {
                final Property prop = kobject.getProperty(transaction, Constraint.TYPE);

                if (UniqueConstraint.CONSTRAINT_TYPE.toString().equals(prop.getStringValue())) {
                    final UniqueConstraint constraint = new UniqueConstraintImpl(getRepository(), kobject.getAbsolutePath());

                    if (LOGGER.isDebugEnabled()) {
                        LOGGER.debug("getUniqueContraints: transaction = '{0}', found unique constraint = '{1}'", //$NON-NLS-1$
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
            LOGGER.debug("removeAccessPattern: transaction = '{0}', accessPatternToRemove = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         accessPatternToRemove);
        }

        boolean found = false;

        try {
            for (final KomodoObject kobject : getChildrenOfType(transaction, Constraint.TABLE_ELEMENT)) {
                final Property prop = kobject.getProperty(transaction, Constraint.TYPE);

                if (AccessPattern.CONSTRAINT_TYPE.toString().equals(prop.getStringValue())
                    && accessPatternToRemove.equals(kobject.getName(transaction))) {
                    removeChild(transaction, accessPatternToRemove);
                    found = true;
                    break;
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
            LOGGER.debug("removeColumn: transaction = '{0}', columnToRemove = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         columnToRemove);
        }

        boolean found = false;

        try {
            for (final KomodoObject kobject : getChildrenOfType(transaction, CreateTable.TABLE_ELEMENT)) {
                if (columnToRemove.equals(kobject.getName(transaction))) {
                    removeChild(transaction, columnToRemove);
                    found = true;
                    break;
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
            LOGGER.debug("removeForeignKey: transaction = '{0}', foreignKeyToRemove = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         foreignKeyToRemove);
        }

        boolean found = false;

        try {
            for (final KomodoObject kobject : getChildrenOfType(transaction, Constraint.FOREIGN_KEY_CONSTRAINT)) {
                if (foreignKeyToRemove.equals(kobject.getName(transaction))) {
                    removeChild(transaction, foreignKeyToRemove);
                    found = true;
                    break;
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
            LOGGER.debug("removeIndex: transaction = '{0}', indexToRemove = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         indexToRemove);
        }

        boolean found = false;

        try {
            for (final KomodoObject kobject : getChildrenOfType(transaction, Constraint.INDEX_CONSTRAINT)) {
                if (indexToRemove.equals(kobject.getName(transaction))) {
                    removeChild(transaction, indexToRemove);
                    found = true;
                    break;
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
    public void removePrimaryKey( UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-removePrimaryKey", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removePrimaryKey: transaction = '{0}'", transaction.getName()); //$NON-NLS-1$
        }

        boolean found = false;

        try {
            for (final KomodoObject kobject : getChildrenOfType(transaction, Constraint.TABLE_ELEMENT)) {
                final Property prop = kobject.getProperty(transaction, Constraint.TYPE);

                if (PrimaryKey.CONSTRAINT_TYPE.toString().equals(prop.getStringValue())) {
                    removeChild(transaction, kobject.getName(transaction));
                    found = true;
                    break;
                }
            }

            if (!found) {
                throw new KException(Messages.getString(Relational.CONSTRAINT_NOT_FOUND_TO_REMOVE,
                                                        PrimaryKey.CONSTRAINT_TYPE.toString(),
                                                        PrimaryKey.CONSTRAINT_TYPE.toString()));
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
     * @see org.komodo.relational.model.Table#removeStatementOption(org.komodo.spi.repository.Repository.UnitOfWork,
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
            LOGGER.debug("removeStatementOption: transaction = '{0}', optionToRemove = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         optionToRemove);
        }

        boolean found = false;

        try {
            for (final KomodoObject kobject : getChildrenOfType(transaction, StandardDdlLexicon.TYPE_STATEMENT_OPTION)) {
                if (optionToRemove.equals(kobject.getName(transaction))) {
                    removeChild(transaction, optionToRemove);
                    found = true;
                    break;
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
            LOGGER.debug("removeUniqueConstraint: transaction = '{0}', constraintToRemove = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         constraintToRemove);
        }

        boolean found = false;

        try {
            for (final KomodoObject kobject : getChildrenOfType(transaction, Constraint.TABLE_ELEMENT)) {
                final Property prop = kobject.getProperty(transaction, Constraint.TYPE);

                if (UniqueConstraint.CONSTRAINT_TYPE.toString().equals(prop.getStringValue())
                    && constraintToRemove.equals(kobject.getName(transaction))) {
                    removeChild(transaction, constraintToRemove);
                    found = true;
                    break;
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
     * @see org.komodo.relational.model.Table#setOnCommitValue(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.RelationalConstants.OnCommit)
     */
    @Override
    public void setOnCommitValue( final UnitOfWork uow,
                                  final OnCommit newOnCommit ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-setOnCommitValue", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setOnCommitValue: transaction = '{0}', newOnCommit = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newOnCommit);
        }

        try {
            setProperty(transaction, StandardDdlLexicon.ON_COMMIT_VALUE, (newOnCommit == null) ? null : newOnCommit.toString());

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
            LOGGER.debug("setPrimaryKey: transaction = '{0}', newPrimaryKeyName = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newPrimaryKeyName);
        }

        try {
            // delete existing primary key
            final PrimaryKey primaryKey = getPrimaryKey(transaction);

            if (primaryKey != null) {
                removeChild(transaction, primaryKey.getName(transaction));
            }

            final PrimaryKey result = RelationalModelFactory.createPrimaryKey(transaction, getRepository(), this, newPrimaryKeyName);

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
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-setQueryExpression", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setQueryExpression: transaction = '{0}', newQueryExpression = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newQueryExpression);
        }

        try {
            setProperty(transaction,
                        CreateTable.QUERY_EXPRESSION,
                        StringUtils.isBlank(newQueryExpression) ? null : newQueryExpression);

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
     * @see org.komodo.relational.model.SchemaElement#setSchemaElementType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.model.SchemaElement.SchemaElementType)
     */
    @Override
    public void setSchemaElementType( final UnitOfWork uow,
                                      final SchemaElementType newSchemaElementType ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-setSchemaElementType", false, null); //$NON-NLS-1$
        }

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setSchemaElementType: transaction = '{0}', newSchemaElementType = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newSchemaElementType);
        }

        try {
            setProperty(transaction,
                        SchemaElement.TYPE.toString(),
                        (newSchemaElementType == null) ? SchemaElementType.DEFAULT_VALUE : newSchemaElementType.toString());

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
     * @see org.komodo.relational.model.Table#setTemporaryTableType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.RelationalConstants.TemporaryType)
     */
    @Override
    public void setTemporaryTableType( final UnitOfWork uow,
                                       final TemporaryType newTempType ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("tableimpl-setTemporaryTableType", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setTemporaryTableType: transaction = '{0}', newTempType = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newTempType);
        }

        try {
            setProperty(transaction, StandardDdlLexicon.TEMPORARY, (newTempType == null) ? null : newTempType.toString());

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

}
