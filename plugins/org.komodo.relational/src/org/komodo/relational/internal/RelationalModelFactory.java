/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.internal;

import org.komodo.relational.model.AccessPattern;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Index;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.OptionContainer;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.ProcedureResultSet;
import org.komodo.relational.model.RelationalObject;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.relational.model.View;
import org.komodo.relational.model.internal.AccessPatternImpl;
import org.komodo.relational.model.internal.ColumnImpl;
import org.komodo.relational.model.internal.ForeignKeyImpl;
import org.komodo.relational.model.internal.IndexImpl;
import org.komodo.relational.model.internal.ParameterImpl;
import org.komodo.relational.model.internal.PrimaryKeyImpl;
import org.komodo.relational.model.internal.ProcedureImpl;
import org.komodo.relational.model.internal.ProcedureResultSetImpl;
import org.komodo.relational.model.internal.StatementOptionImpl;
import org.komodo.relational.model.internal.TableImpl;
import org.komodo.relational.model.internal.UniqueConstraintImpl;
import org.komodo.relational.model.internal.ViewImpl;
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Entry;
import org.komodo.relational.vdb.Mask;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.relational.vdb.internal.ConditionImpl;
import org.komodo.relational.vdb.internal.DataRoleImpl;
import org.komodo.relational.vdb.internal.EntryImpl;
import org.komodo.relational.vdb.internal.MaskImpl;
import org.komodo.relational.vdb.internal.PermissionImpl;
import org.komodo.relational.vdb.internal.TranslatorImpl;
import org.komodo.relational.vdb.internal.VdbImpl;
import org.komodo.relational.vdb.internal.VdbImportImpl;
import org.komodo.repository.RepositoryTools;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.Constraint;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateTable;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

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
            transaction = repository.createTransaction("relationalmodelfactory-createAccessPattern", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject kobject = repository.add(transaction, parentTable.getAbsolutePath(), accessPatternName, null);
            kobject.addDescriptor(transaction, Constraint.TABLE_ELEMENT);
            kobject.setProperty(transaction, Constraint.TYPE, AccessPattern.CONSTRAINT_TYPE.toString());

            final AccessPattern result = new AccessPatternImpl(transaction, repository, kobject.getAbsolutePath());

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param table
     *        the table where container where the column is being created (cannot be <code>null</code>)
     * @param columnName
     *        the name of the column to create (cannot be empty)
     * @return the column model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Column createColumn( final UnitOfWork uow,
                                       final Repository repository,
                                       final Table table,
                                       final String columnName ) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(table, "table"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(columnName, "columnName"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("relationalmodelfactory-createColumn", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject kobject = repository.add(transaction, table.getAbsolutePath(), columnName, null);
            kobject.addDescriptor(transaction, CreateTable.TABLE_ELEMENT);

            final Column result = new ColumnImpl(transaction, repository, kobject.getAbsolutePath());

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentPermission
     *        the permission where the condition model object is being created (cannot be <code>null</code>)
     * @param conditionName
     *        the name of the condition to create (cannot be empty)
     * @return the condition model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Condition createCondition( final UnitOfWork uow,
                                             final Repository repository,
                                             final Permission parentPermission,
                                             final String conditionName ) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentPermission, "parentPermission"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(conditionName, "conditionName"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("relationalmodelfactory-createCondition", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject grouping = RepositoryTools.findOrCreateChild(transaction,
                                                                            parentPermission,
                                                                            VdbLexicon.DataRole.Permission.CONDITIONS,
                                                                            VdbLexicon.DataRole.Permission.CONDITIONS);
            final KomodoObject kobject = grouping.addChild(transaction,
                                                           conditionName,
                                                           VdbLexicon.DataRole.Permission.Condition.CONDITION);
            final Condition result = new ConditionImpl(transaction, repository, kobject.getAbsolutePath());

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentVdb
     *        the VDB where the data role model object is being created (cannot be <code>null</code>)
     * @param dataRoleName
     *        the name of the data role to create (cannot be empty)
     * @return the data role model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static DataRole createDataRole( final UnitOfWork uow,
                                           final Repository repository,
                                           final Vdb parentVdb,
                                           final String dataRoleName ) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentVdb, "parentVdb"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(dataRoleName, "dataRoleName"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("relationalmodelfactory-createDataRole", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject grouping = RepositoryTools.findOrCreateChild(transaction,
                                                                            parentVdb,
                                                                            VdbLexicon.Vdb.DATA_ROLES,
                                                                            VdbLexicon.Vdb.DATA_ROLES);
            final KomodoObject kobject = grouping.addChild(transaction, dataRoleName, VdbLexicon.DataRole.DATA_ROLE);
            final DataRole result = new DataRoleImpl(transaction, repository, kobject.getAbsolutePath());

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentVdb
     *        the VDB where the entry model object is being created (cannot be <code>null</code>)
     * @param entryName
     *        the name of the VDB entry to create (cannot be empty)
     * @param entryPath
     *        the imported VDB path (cannot be empty)
     * @return the VDB entry model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Entry createEntry( final UnitOfWork uow,
                                     final Repository repository,
                                     final Vdb parentVdb,
                                     final String entryName,
                                     final String entryPath ) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentVdb, "parentVdb"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(entryName, "entryName"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(entryPath, "entryPath"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("relationalmodelfactory-createEntry", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject grouping = RepositoryTools.findOrCreateChild(transaction,
                                                                            parentVdb,
                                                                            VdbLexicon.Vdb.ENTRIES,
                                                                            VdbLexicon.Vdb.ENTRIES);
            final KomodoObject kobject = grouping.addChild(transaction, entryName, VdbLexicon.Entry.ENTRY);
            final Entry result = new EntryImpl(transaction, repository, kobject.getAbsolutePath());
            result.setPath(transaction, entryPath);

            if (uow == null) {
                transaction.commit();
            }

            return result;
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
     * @throws KException
     *         if an error occurs
     */
    public static ForeignKey createForeignKey( final UnitOfWork uow,
                                               final Repository repository,
                                               final Table parentTable,
                                               final String foreignKeyName,
                                               final Table tableReference ) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentTable, "parentTable"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(foreignKeyName, "foreignKeyName"); //$NON-NLS-1$
        ArgCheck.isNotNull(tableReference, "tableReference"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("relationalmodelfactory-createForeignKey", false, null); //$NON-NLS-1$
        }

        try {
            final KomodoObject kobject = repository.add(transaction, parentTable.getAbsolutePath(), foreignKeyName, null);
            kobject.addDescriptor(transaction, Constraint.FOREIGN_KEY_CONSTRAINT);

            final ForeignKey fk = new ForeignKeyImpl(transaction, repository, kobject.getAbsolutePath());
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
     * @param parentModel
     *        the model where the function is being created (cannot be <code>null</code>)
     * @param functionName
     *        the name of the function to create (cannot be empty)
     * @return the function model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Procedure createFunction( final UnitOfWork uow,
                                            final Repository repository,
                                            final Model parentModel,
                                            final String functionName ) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentModel, "parentModel"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(functionName, "functionName"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("relationalmodelfactory-createFunction", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final Procedure result = createProcedure(transaction, repository, parentModel, functionName);
            result.setFunction(transaction, true);

            if (uow == null) {
                transaction.commit();
            }

            return result;
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
     * @throws KException
     *         if an error occurs
     */
    public static Index createIndex( final UnitOfWork uow,
                                     final Repository repository,
                                     final Table parentTable,
                                     final String indexName ) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentTable, "parentTable"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(indexName, "indexName"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("relationalmodelfactory-createIndex", false, null); //$NON-NLS-1$
        }

        try {
            final KomodoObject kobject = repository.add(transaction, parentTable.getAbsolutePath(), indexName, null);
            kobject.addDescriptor(transaction, Constraint.INDEX_CONSTRAINT.toString());

            final Index index = new IndexImpl(transaction, repository, kobject.getAbsolutePath());

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
     * @param parentPermission
     *        the permission where the mask model object is being created (cannot be <code>null</code>)
     * @param maskName
     *        the name of the mask to create (cannot be empty)
     * @return the mask model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Mask createMask( final UnitOfWork uow,
                                   final Repository repository,
                                   final Permission parentPermission,
                                   final String maskName ) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentPermission, "parentPermission"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(maskName, "maskName"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("relationalmodelfactory-createMask", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject grouping = RepositoryTools.findOrCreateChild(transaction,
                                                                            parentPermission,
                                                                            VdbLexicon.DataRole.Permission.MASKS,
                                                                            VdbLexicon.DataRole.Permission.MASKS);
            final KomodoObject kobject = grouping.addChild(transaction, maskName, VdbLexicon.DataRole.Permission.Mask.MASK);
            final Mask result = new MaskImpl(transaction, repository, kobject.getAbsolutePath());

            if (uow == null) {
                transaction.commit();
            }

            return result;
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
     * @throws KException
     *         if an error occurs
     */
    public static Parameter createParameter( final UnitOfWork uow,
                                             final Repository repository,
                                             final Procedure parentProcedure,
                                             final String parameterName ) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentProcedure, "parentProcedure"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(parameterName, "parameterName"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("relationalmodelfactory-createParameter", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject kobject = repository.add(transaction, parentProcedure.getAbsolutePath(), parameterName, null);
            kobject.addDescriptor(transaction, CreateProcedure.PARAMETER);

            final Parameter result = new ParameterImpl(transaction, repository, kobject.getAbsolutePath());

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentDataRole
     *        the data role where the permission model object is being created (cannot be <code>null</code>)
     * @param permissionName
     *        the name of the permission to create (cannot be empty)
     * @return the permission model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Permission createPermission( final UnitOfWork uow,
                                               final Repository repository,
                                               final DataRole parentDataRole,
                                               final String permissionName ) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentDataRole, "parentDataRole"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(permissionName, "permissionName"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("relationalmodelfactory-createPermission", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject grouping = RepositoryTools.findOrCreateChild(transaction,
                                                                            parentDataRole,
                                                                            VdbLexicon.DataRole.PERMISSIONS,
                                                                            VdbLexicon.DataRole.PERMISSIONS);
            final KomodoObject kobject = grouping.addChild(transaction, permissionName, VdbLexicon.DataRole.Permission.PERMISSION);
            final Permission result = new PermissionImpl(transaction, repository, kobject.getAbsolutePath());

            if (uow == null) {
                transaction.commit();
            }

            return result;
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
            transaction = repository.createTransaction("relationalmodelfactory-createPrimaryKey", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject kobject = repository.add(transaction, parentTable.getAbsolutePath(), primaryKeyName, null);
            kobject.addDescriptor(transaction, Constraint.TABLE_ELEMENT);
            kobject.setProperty(transaction, Constraint.TYPE, PrimaryKey.CONSTRAINT_TYPE.toString());

            final PrimaryKey result = new PrimaryKeyImpl(transaction, repository, kobject.getAbsolutePath());

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentModel
     *        the model where the procedure is being created (cannot be <code>null</code>)
     * @param procedureName
     *        the name of the procedure to create (cannot be empty)
     * @return the procedure model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Procedure createProcedure( final UnitOfWork uow,
                                             final Repository repository,
                                             final Model parentModel,
                                             final String procedureName ) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentModel, "parentModel"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(procedureName, "procedureName"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("relationalmodelfactory-createProcedure", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject kobject = repository.add(transaction, parentModel.getAbsolutePath(), procedureName, null);
            kobject.addDescriptor(transaction, CreateProcedure.PROCEDURE_STATEMENT);
            setCreateStatementProperties(transaction, kobject);

            final Procedure result = new ProcedureImpl(transaction, repository, kobject.getAbsolutePath());

            if (uow == null) {
                transaction.commit();
            }

            return result;
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
     * @return the procedure result set model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static ProcedureResultSet createProcedureResultSet( final UnitOfWork uow,
                                                               final Repository repository,
                                                               final Procedure parentProcedure ) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentProcedure, "parentProcedure"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("relationalmodelfactory-createProcedureResultSet", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject kobject = repository.add(transaction,
                                                        parentProcedure.getAbsolutePath(),
                                                        CreateProcedure.RESULT_SET,
                                                        null);
            kobject.addDescriptor(transaction, CreateProcedure.RESULT_DATA_TYPE);

            final ProcedureResultSet result = new ProcedureResultSetImpl(transaction, repository, kobject.getAbsolutePath());

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param optionContainer
     *        the parent where the option is being created (can be empty if created at the root of the workspace)
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
                                                         final OptionContainer optionContainer,
                                                         final String optionName,
                                                         final String optionValue ) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(optionContainer, "optionContainer"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(optionName, "optionName"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(optionValue, "optionValue"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("relationalmodelfactory-createStatementOption", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject kobject = repository.add(transaction, optionContainer.getAbsolutePath(), optionName, null);
            kobject.addDescriptor(transaction, StandardDdlLexicon.TYPE_STATEMENT_OPTION);

            final StatementOption result = new StatementOptionImpl(transaction, repository, kobject.getAbsolutePath());
            result.setOption(transaction, optionValue);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentModel
     *        the model where the table is being created (cannot be <code>null</code>)
     * @param tableName
     *        the name of the table to create (cannot be empty)
     * @return the table model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Table createTable( final UnitOfWork uow,
                                     final Repository repository,
                                     final Model parentModel,
                                     final String tableName ) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentModel, "parentModel"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(tableName, "tableName"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("relationalmodelfactory-createTable", false, null); //$NON-NLS-1$
        }

        try {
            final KomodoObject kobject = repository.add(transaction, parentModel.getAbsolutePath(), tableName, null);
            kobject.addDescriptor(transaction, CreateTable.TABLE_STATEMENT);
            setCreateStatementProperties(transaction, kobject);

            final Table result = new TableImpl(transaction, repository, kobject.getAbsolutePath());

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentVdb
     *        the VDB where the VDB import model object is being created (cannot be <code>null</code>)
     * @param translatorName
     *        the name of the VDB translator to create (cannot be empty)
     * @param translatorType
     *        the type of translator (cannot be empty)
     * @return the VDB translator model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Translator createTranslator( final UnitOfWork uow,
                                               final Repository repository,
                                               final Vdb parentVdb,
                                               final String translatorName,
                                               final String translatorType ) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentVdb, "parentVdb"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(translatorName, "translatorName"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(translatorType, "translatorType"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("relationalmodelfactory-createTranslator", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject grouping = RepositoryTools.findOrCreateChild(transaction,
                                                                            parentVdb,
                                                                            VdbLexicon.Vdb.TRANSLATORS,
                                                                            VdbLexicon.Vdb.TRANSLATORS);
            final KomodoObject kobject = grouping.addChild(transaction, translatorName, VdbLexicon.Translator.TRANSLATOR);
            final Translator result = new TranslatorImpl(transaction, repository, kobject.getAbsolutePath());
            result.setType(transaction, translatorType);

            if (uow == null) {
                transaction.commit();
            }

            return result;
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
     * @return the unique constraint model object (never <code>null</code>)
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
            transaction = repository.createTransaction("relationalmodelfactory-createUniqueConstraint", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject kobject = repository.add(transaction, parentTable.getAbsolutePath(), uniqueConstraintName, null);
            kobject.addDescriptor(transaction, Constraint.TABLE_ELEMENT);
            kobject.setProperty(transaction, Constraint.TYPE, UniqueConstraint.CONSTRAINT_TYPE);

            final UniqueConstraint result = new UniqueConstraintImpl(transaction, repository, kobject.getAbsolutePath());

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentWorkspacePath
     *        the parent path (can be empty)
     * @param vdbName
     *        the name of the VDB to create (cannot be empty)
     * @param externalFilePath
     *        the VDB file path on the local file system (cannot be empty)
     * @return the VDB model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Vdb createVdb( final UnitOfWork uow,
                                 final Repository repository,
                                 final String parentWorkspacePath,
                                 final String vdbName,
                                 final String externalFilePath ) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(vdbName, "vdbName"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(externalFilePath, "externalFilePath"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("relationalmodelfactory-createVdb", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject kobject = repository.add(transaction,
                                                        parentWorkspacePath,
                                                        vdbName,
                                                        VdbLexicon.Vdb.VIRTUAL_DATABASE);
            final Vdb result = new VdbImpl(transaction, repository, kobject.getAbsolutePath());
            result.setOriginalFilePath(transaction, externalFilePath);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentVdb
     *        the VDB where the VDB import model object is being created (cannot be <code>null</code>)
     * @param vdbName
     *        the name of the VDB import to create (cannot be empty)
     * @return the VDB import model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static VdbImport createVdbImport( final UnitOfWork uow,
                                             final Repository repository,
                                             final Vdb parentVdb,
                                             final String vdbName ) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentVdb, "parentVdb"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(vdbName, "vdbName"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("relationalmodelfactory-createVdbImport", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject grouping = RepositoryTools.findOrCreateChild(transaction,
                                                                            parentVdb,
                                                                            VdbLexicon.Vdb.IMPORT_VDBS,
                                                                            VdbLexicon.Vdb.IMPORT_VDBS);
            final KomodoObject kobject = grouping.addChild(transaction, vdbName, VdbLexicon.ImportVdb.IMPORT_VDB);
            final VdbImport result = new VdbImportImpl(transaction, repository, kobject.getAbsolutePath());
            result.setVersion(transaction, Vdb.DEFAULT_VERSION);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentModel
     *        the model where the view is being created (cannot be <code>null</code>)
     * @param viewName
     *        the name of the view to create (cannot be empty)
     * @return the view model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static View createView( final UnitOfWork uow,
                                   final Repository repository,
                                   final Model parentModel,
                                   final String viewName ) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentModel, "parentModel"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(viewName, "viewName"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = repository.createTransaction("relationalmodelfactory-createView", false, null); //$NON-NLS-1$
        }

        try {
            final KomodoObject kobject = repository.add(transaction, parentModel.getAbsolutePath(), viewName, null);
            kobject.addDescriptor(transaction, CreateTable.VIEW_STATEMENT);
            setCreateStatementProperties(transaction, kobject);

            final View result = new ViewImpl(transaction, repository, kobject.getAbsolutePath());

            if (uow == null) {
                transaction.commit();
            }

            return result;
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
