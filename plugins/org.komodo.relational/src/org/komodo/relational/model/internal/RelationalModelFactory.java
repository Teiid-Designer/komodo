/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import org.komodo.relational.model.AccessPattern;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Index;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.ProcedureResultSet;
import org.komodo.relational.model.RelationalObject;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.relational.model.View;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.Constraint;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateTable;

/**
 * A factory for {@link RelationalObject relational model objects}.
 */
public final class RelationalModelFactory {

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentTable
     *        the parent of the model object being created (cannot be <code>null</code>)
     * @param accessPatternName
     *        the name of the access pattern to create (cannot be empty)
     * @return the access pattern model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static AccessPattern createAccessPattern( final UnitOfWork uow,
                                                     final Repository repository,
                                                     final Table parentTable,
                                                     final String accessPatternName ) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentTable, "parentTable"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(accessPatternName, "accessPatternName"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("komodofactory-createAccessPattern", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject kobject = repository.add(transaction, parentTable.getAbsolutePath(), accessPatternName, null);
            kobject.addDescriptor(transaction, Constraint.TABLE_ELEMENT);
            kobject.setProperty(transaction, Constraint.TYPE, AccessPattern.CONSTRAINT_TYPE);

            if (uow == null) {
                transaction.commit();
            }

            return new AccessPatternImpl(repository, kobject.getAbsolutePath());
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentPath
     *        the path to the parent of the model object being created (can be empty if created at the root of the workspace)
     * @param columnName
     *        the name of the column to create (cannot be empty)
     * @return the column model object (never <code>null</code>)
     * @throws Exception
     *         if an error occurs
     */
    public static Column createColumn( final UnitOfWork uow,
                                       final Repository repository,
                                       final String parentPath,
                                       final String columnName ) throws Exception {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(columnName, "columnName"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("komodofactory-createColumn", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject kobject = repository.add(transaction, parentPath, columnName, null);
            kobject.addDescriptor(transaction, CreateTable.TABLE_ELEMENT);

            if (uow == null) {
                transaction.commit();
            }

            return new ColumnImpl(repository, kobject.getAbsolutePath());
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentTable
     *        the table model object where the foreign key is being created (cannot be <code>null</code>)
     * @param foreignKeyName
     *        the name of the foreign key to create (cannot be empty)
     * @param tableReference
     *        the table referenced by this constraint (cannot be <code>null</code>)
     * @return the foreign key model object (never <code>null</code>)
     * @throws Exception
     *         if an error occurs
     */
    public static ForeignKey createForeignKey( final UnitOfWork uow,
                                               final Repository repository,
                                               final Table parentTable,
                                               final String foreignKeyName,
                                               final Table tableReference ) throws Exception {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentTable, "parentTable"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(foreignKeyName, "foreignKeyName"); //$NON-NLS-1$
        ArgCheck.isNotNull(tableReference, "tableReference"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("komodofactory-createForeignKey", false, null); //$NON-NLS-1$
        }

        try {
            final KomodoObject kobject = repository.add(transaction, parentTable.getAbsolutePath(), foreignKeyName, null);
            kobject.addDescriptor(transaction, Constraint.FOREIGN_KEY_CONSTRAINT);

            final ForeignKey fk = new ForeignKeyImpl(repository, kobject.getAbsolutePath());
            fk.setReferencesTable(transaction, tableReference);

            if (uow == null) {
                transaction.commit();
            }

            return fk;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentTable
     *        the table where the index model object is being created (cannot be <code>null</code>)
     * @param indexName
     *        the name of the index to create (cannot be empty)
     * @return the index model object (never <code>null</code>)
     * @throws Exception
     *         if an error occurs
     */
    public static Index createIndex( final UnitOfWork uow,
                                     final Repository repository,
                                     final Table parentTable,
                                     final String indexName ) throws Exception {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentTable, "parentTable"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(indexName, "indexName"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("komodofactory-createIndex", false, null); //$NON-NLS-1$
        }

        try {
            final KomodoObject kobject = repository.add(transaction, parentTable.getAbsolutePath(), indexName, null);
            kobject.addDescriptor(transaction, Constraint.INDEX_CONSTRAINT);

            final Index index = new IndexImpl(repository, kobject.getAbsolutePath());

            if (uow == null) {
                transaction.commit();
            }

            return index;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentProcedure
     *        the procedure where the parameter model object is being created (cannot be <code>null</code>)
     * @param parameterName
     *        the name of the parameter to create (cannot be empty)
     * @return the parameter model object (never <code>null</code>)
     * @throws Exception
     *         if an error occurs
     */
    public static Parameter createParameter( final UnitOfWork uow,
                                             final Repository repository,
                                             final Procedure parentProcedure,
                                             final String parameterName ) throws Exception {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentProcedure, "parentProcedure"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(parameterName, "parameterName"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("komodofactory-createParameter", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject kobject = repository.add(transaction, parentProcedure.getAbsolutePath(), parameterName, null);
            kobject.addDescriptor(transaction, CreateProcedure.PARAMETER);

            if (uow == null) {
                transaction.commit();
            }

            return new ParameterImpl(repository, kobject.getAbsolutePath());
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentTable
     *        the parent of the model object being created (cannot be <code>null</code>)
     * @param primaryKeyName
     *        the name of the primary key to create (cannot be empty)
     * @return the primary key model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static PrimaryKey createPrimaryKey( final UnitOfWork uow,
                                               final Repository repository,
                                               final Table parentTable,
                                               final String primaryKeyName ) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentTable, "parentTable"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(primaryKeyName, "primaryKeyName"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("komodofactory-createPrimaryKey", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject kobject = repository.add(transaction, parentTable.getAbsolutePath(), primaryKeyName, null);
            kobject.addDescriptor(transaction, Constraint.TABLE_ELEMENT);
            kobject.setProperty(transaction, Constraint.TYPE, PrimaryKey.CONSTRAINT_TYPE);

            if (uow == null) {
                transaction.commit();
            }

            return new PrimaryKeyImpl(repository, kobject.getAbsolutePath());
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentPath
     *        the path to the parent of the model object being created (can be empty if created at the root of the workspace)
     * @param procedureName
     *        the name of the procedure to create (cannot be empty)
     * @return the procedure model object (never <code>null</code>)
     * @throws Exception
     *         if an error occurs
     */
    public static Procedure createProcedure( final UnitOfWork uow,
                                             final Repository repository,
                                             final String parentPath,
                                             final String procedureName ) throws Exception {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(procedureName, "procedureName"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("komodofactory-createProcedure", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject kobject = repository.add(transaction, parentPath, procedureName, null);
            kobject.addDescriptor(transaction, CreateProcedure.PROCEDURE_STATEMENT);
            setCreateStatementProperties(transaction, kobject);

            if (uow == null) {
                transaction.commit();
            }

            return new ProcedureImpl(repository, kobject.getAbsolutePath());
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentProcedure
     *        the procedure where the procedure result set model object is being created (cannot be <code>null</code>)
     * @return the parameter model object (never <code>null</code>)
     * @throws Exception
     *         if an error occurs
     */
    public static ProcedureResultSet createProcedureResultSet( final UnitOfWork uow,
                                                               final Repository repository,
                                                               final Procedure parentProcedure ) throws Exception {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentProcedure, "parentProcedure"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("komodofactory-createProcedureResultSet", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject kobject = repository.add(transaction,
                                                        parentProcedure.getAbsolutePath(),
                                                        CreateProcedure.RESULT_SET,
                                                        null);
            kobject.addDescriptor(transaction, CreateProcedure.RESULT_DATA_TYPE);

            if (uow == null) {
                transaction.commit();
            }

            return new ProcedureResultSetImpl(repository, kobject.getAbsolutePath());
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentPath
     *        the path to the parent of the model object being created (can be empty if created at the root of the workspace)
     * @param optionName
     *        the name of the statement option to create (cannot be empty)
     * @param optionValue
     *        the option value (cannot be empty)
     * @return the statement option model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static StatementOption createStatementOption( final UnitOfWork uow,
                                                         final Repository repository,
                                                         final String parentPath,
                                                         final String optionName,
                                                         final String optionValue ) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(optionName, "optionName"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(optionValue, "optionValue"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("komodofactory-createStatementOption", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject kobject = repository.add(transaction, parentPath, optionName, null);
            kobject.addDescriptor(transaction, StandardDdlLexicon.TYPE_STATEMENT_OPTION);
            kobject.setProperty(transaction, StandardDdlLexicon.VALUE, optionValue);

            if (uow == null) {
                transaction.commit();
            }

            return new StatementOptionImpl(repository, kobject.getAbsolutePath());
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    // TODO parameter
    // TODO procedureresultset

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentPath
     *        the path to the parent of the model object being created (can be empty if created at the root of the workspace)
     * @param tableName
     *        the name of the table to create (cannot be empty)
     * @return the table model object (never <code>null</code>)
     * @throws Exception
     *         if an error occurs
     */
    public static Table createTable( final UnitOfWork uow,
                                     final Repository repository,
                                     final String parentPath,
                                     final String tableName ) throws Exception {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(tableName, "tableName"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("komodofactory-createTable", false, null); //$NON-NLS-1$
        }

        try {
            final KomodoObject kobject = repository.add(transaction, parentPath, tableName, null);
            kobject.addDescriptor(transaction, CreateTable.TABLE_STATEMENT);
            setCreateStatementProperties(transaction, kobject);

            if (uow == null) {
                transaction.commit();
            }

            return new TableImpl(repository, kobject.getAbsolutePath());
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentTable
     *        the parent of the model object being created (cannot be <code>null</code>)
     * @param uniqueConstraintName
     *        the name of the unique constraint to create (cannot be empty)
     * @return the primary key model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static UniqueConstraint createUniqueConstraint( final UnitOfWork uow,
                                                           final Repository repository,
                                                           final Table parentTable,
                                                           final String uniqueConstraintName ) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentTable, "parentTable"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(uniqueConstraintName, "uniqueConstraintName"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("komodofactory-createUniqueConstraint", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject kobject = repository.add(transaction, parentTable.getAbsolutePath(), uniqueConstraintName, null);
            kobject.addDescriptor(transaction, Constraint.TABLE_ELEMENT);
            kobject.setProperty(transaction, Constraint.TYPE, UniqueConstraint.CONSTRAINT_TYPE);

            if (uow == null) {
                transaction.commit();
            }

            return new UniqueConstraintImpl(repository, kobject.getAbsolutePath());
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentPath
     *        the path to the parent of the model object being created (can be empty if created at the root of the workspace)
     * @param viewName
     *        the name of the view to create (cannot be empty)
     * @return the view model object (never <code>null</code>)
     * @throws Exception
     *         if an error occurs
     */
    public static View createView( final UnitOfWork uow,
                                   final Repository repository,
                                   final String parentPath,
                                   final String viewName ) throws Exception {
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("komodofactory-createView", false, null); //$NON-NLS-1$
        }

        try {
            final KomodoObject kobject = repository.add(transaction, parentPath, viewName, null);
            kobject.addDescriptor(transaction, CreateTable.VIEW_STATEMENT);
            setCreateStatementProperties(transaction, kobject);

            if (uow == null) {
                transaction.commit();
            }

            return new ViewImpl(repository, kobject.getAbsolutePath());
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * Only one of the {@link UnitOfWork transactions} passed in should be non-<code>null</code>. Ensures that a transaction
     * rollback occurs if the transaction was constructed within the method.
     *
     * @param transactionParameter
     *        the transaction passed into the method (can be <code>null</code>)
     * @param transactionVariable
     *        the transaction constructed within the method (can be <code>null</code>)
     * @param e
     *        the error being handled (cannot be <code>null</code>)
     * @return the error passed in if already a {@link KException} or the error passed in wrapped in a {@link KException}
     */
    private static KException handleError( final UnitOfWork transactionParameter,
                                           final UnitOfWork transactionVariable,
                                           final Exception e ) {
        assert (e != null);
        assert ((transactionParameter == null) && (transactionVariable != null))
        || ((transactionParameter != null) && (transactionVariable == null));

        if (transactionParameter == null) {
            transactionVariable.rollback();
        }

        if (e instanceof KException) {
            return (KException)e;
        }

        return new KException(e);
    }

    private static void setCreateStatementProperties( final UnitOfWork transaction,
                                                      final KomodoObject kobject ) throws KException {
        /*

        - ddl:expression (string) mandatory             // The string fragment encompassing the statement expression.
        - ddl:originalExpression (string)               // The string fragment encompassing the original statement expression.
        - ddl:startLineNumber (long) mandatory          // The starting line number for the statement
        - ddl:startColumnNumber (long) mandatory        // The starting column number for the statement
        - ddl:startCharIndex (long) mandatory           // The starting content character index for the statement
        - ddl:length (long)  mandatory                  // The string length

         */

        kobject.setProperty(transaction, StandardDdlLexicon.DDL_EXPRESSION, "komodo created model"); //$NON-NLS-1$
        kobject.setProperty(transaction, StandardDdlLexicon.DDL_ORIGINAL_EXPRESSION, "komodo created model"); //$NON-NLS-1$
        kobject.setProperty(transaction, StandardDdlLexicon.DDL_START_LINE_NUMBER, 0L);
        kobject.setProperty(transaction, StandardDdlLexicon.DDL_START_COLUMN_NUMBER, 0L);
        kobject.setProperty(transaction, StandardDdlLexicon.DDL_START_CHAR_INDEX, 0L);
        kobject.setProperty(transaction, StandardDdlLexicon.DDL_LENGTH, 0L);
    }

    private RelationalModelFactory() {
        // nothing to do
    }

}
