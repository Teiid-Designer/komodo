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
package org.komodo.relational;

import org.komodo.core.KomodoLexicon;
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.datasource.internal.DatasourceImpl;
import org.komodo.relational.model.AbstractProcedure;
import org.komodo.relational.model.AccessPattern;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Index;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.OptionContainer;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.relational.model.ResultSetColumn;
import org.komodo.relational.model.Schema;
import org.komodo.relational.model.SchemaElement.SchemaElementType;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.relational.model.UserDefinedFunction;
import org.komodo.relational.model.View;
import org.komodo.relational.model.VirtualProcedure;
import org.komodo.relational.model.internal.AccessPatternImpl;
import org.komodo.relational.model.internal.ColumnImpl;
import org.komodo.relational.model.internal.DataTypeResultSetImpl;
import org.komodo.relational.model.internal.ForeignKeyImpl;
import org.komodo.relational.model.internal.IndexImpl;
import org.komodo.relational.model.internal.ModelImpl;
import org.komodo.relational.model.internal.ParameterImpl;
import org.komodo.relational.model.internal.PrimaryKeyImpl;
import org.komodo.relational.model.internal.PushdownFunctionImpl;
import org.komodo.relational.model.internal.ResultSetColumnImpl;
import org.komodo.relational.model.internal.SchemaImpl;
import org.komodo.relational.model.internal.StatementOptionImpl;
import org.komodo.relational.model.internal.StoredProcedureImpl;
import org.komodo.relational.model.internal.TableImpl;
import org.komodo.relational.model.internal.TabularResultSetImpl;
import org.komodo.relational.model.internal.UniqueConstraintImpl;
import org.komodo.relational.model.internal.UserDefinedFunctionImpl;
import org.komodo.relational.model.internal.ViewImpl;
import org.komodo.relational.model.internal.VirtualProcedureImpl;
import org.komodo.relational.teiid.CachedTeiid;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.teiid.internal.CachedTeiidImpl;
import org.komodo.relational.teiid.internal.TeiidImpl;
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Entry;
import org.komodo.relational.vdb.Mask;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.relational.vdb.internal.ConditionImpl;
import org.komodo.relational.vdb.internal.DataRoleImpl;
import org.komodo.relational.vdb.internal.EntryImpl;
import org.komodo.relational.vdb.internal.MaskImpl;
import org.komodo.relational.vdb.internal.ModelSourceImpl;
import org.komodo.relational.vdb.internal.PermissionImpl;
import org.komodo.relational.vdb.internal.TranslatorImpl;
import org.komodo.relational.vdb.internal.VdbImpl;
import org.komodo.relational.vdb.internal.VdbImportImpl;
import org.komodo.repository.RepositoryTools;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.Constraint;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.CreateProcedure;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.CreateTable;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.SchemaElement;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * A factory for {@link RelationalObject relational model objects}.
 */
public final class RelationalModelFactory {

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
    public static AccessPattern createAccessPattern( final UnitOfWork transaction,
                                                     final Repository repository,
                                                     final Table parentTable,
                                                     final String accessPatternName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( parentTable, "parentTable" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( accessPatternName, "accessPatternName" ); //$NON-NLS-1$

        final KomodoObject kobject = repository.add( transaction, parentTable.getAbsolutePath(), accessPatternName, null );
        kobject.addDescriptor( transaction, Constraint.TABLE_ELEMENT );
        kobject.setProperty( transaction, Constraint.TYPE, AccessPattern.CONSTRAINT_TYPE.toValue() );

        final AccessPattern result = new AccessPatternImpl( transaction, repository, kobject.getAbsolutePath() );
        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param table
     *        the table where the column is being created (cannot be <code>null</code>)
     * @param columnName
     *        the name of the column to create (cannot be empty)
     * @return the column model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Column createColumn( final UnitOfWork transaction,
                                       final Repository repository,
                                       final Table table,
                                       final String columnName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( table, "table" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( columnName, "columnName" ); //$NON-NLS-1$

        final KomodoObject kobject = repository.add( transaction, table.getAbsolutePath(), columnName, null );
        kobject.addDescriptor( transaction, CreateTable.TABLE_ELEMENT );

        final Column result = new ColumnImpl( transaction, repository, kobject.getAbsolutePath() );
        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
    public static Condition createCondition( final UnitOfWork transaction,
                                             final Repository repository,
                                             final Permission parentPermission,
                                             final String conditionName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( parentPermission, "parentPermission" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( conditionName, "conditionName" ); //$NON-NLS-1$

        try {
            final KomodoObject grouping = RepositoryTools.findOrCreateChild( transaction,
                                                                             parentPermission,
                                                                             VdbLexicon.DataRole.Permission.CONDITIONS,
                                                                             VdbLexicon.DataRole.Permission.CONDITIONS );
            final KomodoObject kobject = grouping.addChild( transaction,
                                                            conditionName,
                                                            VdbLexicon.DataRole.Permission.Condition.CONDITION );
            final Condition result = new ConditionImpl( transaction, repository, kobject.getAbsolutePath() );
            return result;
        } catch ( final Exception e ) {
            throw handleError( e );
        }
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
    public static DataRole createDataRole( final UnitOfWork transaction,
                                           final Repository repository,
                                           final Vdb parentVdb,
                                           final String dataRoleName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentVdb, "parentVdb"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(dataRoleName, "dataRoleName"); //$NON-NLS-1$

        try {
            final KomodoObject grouping = RepositoryTools.findOrCreateChild(transaction,
                                                                            parentVdb,
                                                                            VdbLexicon.Vdb.DATA_ROLES,
                                                                            VdbLexicon.Vdb.DATA_ROLES);
            final KomodoObject kobject = grouping.addChild(transaction, dataRoleName, VdbLexicon.DataRole.DATA_ROLE);
            final DataRole result = new DataRoleImpl(transaction, repository, kobject.getAbsolutePath());
            return result;
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentWorkspacePath
     *        the parent path (can be empty)
     * @param sourceName
     *        the name of the datasource fragment to create (cannot be empty)
     * @return the Datasource model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Datasource createDatasource( final UnitOfWork transaction,
                                               final Repository repository,
                                               final String parentWorkspacePath,
                                               final String sourceName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( sourceName, "sourceName" ); //$NON-NLS-1$

        // make sure path is in the library
        String parentPath = parentWorkspacePath;
        final String workspacePath = repository.komodoWorkspace( transaction ).getAbsolutePath();

        if ( StringUtils.isBlank( parentWorkspacePath ) ) {
            parentPath = workspacePath;
        } else if ( !parentPath.startsWith( workspacePath ) ) {
            parentPath = ( workspacePath + parentPath );
        }

        final KomodoObject kobject = repository.add( transaction, parentPath, sourceName, KomodoLexicon.DataSource.NODE_TYPE );
        final Datasource result = new DatasourceImpl( transaction, repository, kobject.getAbsolutePath() );
        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentProcedure
     *        the procedure where the procedure result set model object is being created (cannot be <code>null</code>)
     * @return the procedure result set model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static DataTypeResultSet createDataTypeResultSet( final UnitOfWork transaction,
                                                             final Repository repository,
                                                             final AbstractProcedure parentProcedure ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( parentProcedure, "parentProcedure" ); //$NON-NLS-1$

        final KomodoObject kobject = repository.add( transaction,
                                                     parentProcedure.getAbsolutePath(),
                                                     CreateProcedure.RESULT_SET,
                                                     null );
        kobject.addDescriptor( transaction, CreateProcedure.RESULT_DATA_TYPE );
        final DataTypeResultSet result = new DataTypeResultSetImpl( transaction, repository, kobject.getAbsolutePath() );
        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
    public static Entry createEntry( final UnitOfWork transaction,
                                     final Repository repository,
                                     final Vdb parentVdb,
                                     final String entryName,
                                     final String entryPath ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentVdb, "parentVdb"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(entryName, "entryName"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(entryPath, "entryPath"); //$NON-NLS-1$

        try {
            final KomodoObject grouping = RepositoryTools.findOrCreateChild(transaction,
                                                                            parentVdb,
                                                                            VdbLexicon.Vdb.ENTRIES,
                                                                            VdbLexicon.Vdb.ENTRIES);
            final KomodoObject kobject = grouping.addChild(transaction, entryName, VdbLexicon.Entry.ENTRY);
            final Entry result = new EntryImpl(transaction, repository, kobject.getAbsolutePath());
            result.setPath(transaction, entryPath);
            return result;
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
    public static ForeignKey createForeignKey( final UnitOfWork transaction,
                                               final Repository repository,
                                               final Table parentTable,
                                               final String foreignKeyName,
                                               final Table tableReference ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( parentTable, "parentTable" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( foreignKeyName, "foreignKeyName" ); //$NON-NLS-1$
        ArgCheck.isNotNull( tableReference, "tableReference" ); //$NON-NLS-1$

        final KomodoObject kobject = repository.add( transaction, parentTable.getAbsolutePath(), foreignKeyName, null );
        kobject.addDescriptor( transaction, Constraint.FOREIGN_KEY_CONSTRAINT );
        kobject.setProperty( transaction, Constraint.TYPE, ForeignKey.CONSTRAINT_TYPE.toValue() );

        final ForeignKey fk = new ForeignKeyImpl( transaction, repository, kobject.getAbsolutePath() );
        fk.setReferencesTable( transaction, tableReference );
        return fk;
    }

    /**
     * This is Teiid's <code>Create Foreign Function</code> command.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
    public static PushdownFunction createPushdownFunction( final UnitOfWork transaction,
                                                           final Repository repository,
                                                           final Model parentModel,
                                                           final String functionName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( parentModel, "parentModel" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( functionName, "functionName" ); //$NON-NLS-1$

        final KomodoObject kobject = repository.add( transaction, parentModel.getAbsolutePath(), functionName, null );
        kobject.addDescriptor( transaction, CreateProcedure.FUNCTION_STATEMENT );
        kobject.setProperty( transaction, SchemaElement.TYPE, SchemaElementType.FOREIGN.name() );
        setCreateStatementProperties( transaction, kobject );

        final PushdownFunction result = new PushdownFunctionImpl( transaction, repository, kobject.getAbsolutePath() );
        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
    public static Index createIndex( final UnitOfWork transaction,
                                     final Repository repository,
                                     final Table parentTable,
                                     final String indexName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( parentTable, "parentTable" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( indexName, "indexName" ); //$NON-NLS-1$

        final KomodoObject kobject = repository.add( transaction, parentTable.getAbsolutePath(), indexName, null );
        kobject.addDescriptor( transaction, Constraint.INDEX_CONSTRAINT );
        kobject.setProperty( transaction, Constraint.TYPE, Index.CONSTRAINT_TYPE.toValue() );

        final Index index = new IndexImpl( transaction, repository, kobject.getAbsolutePath() );
        return index;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
    public static Mask createMask( final UnitOfWork transaction,
                                   final Repository repository,
                                   final Permission parentPermission,
                                   final String maskName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentPermission, "parentPermission"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(maskName, "maskName"); //$NON-NLS-1$

        try {
            final KomodoObject grouping = RepositoryTools.findOrCreateChild(transaction,
                                                                            parentPermission,
                                                                            VdbLexicon.DataRole.Permission.MASKS,
                                                                            VdbLexicon.DataRole.Permission.MASKS);
            final KomodoObject kobject = grouping.addChild(transaction, maskName, VdbLexicon.DataRole.Permission.Mask.MASK);
            final Mask result = new MaskImpl(transaction, repository, kobject.getAbsolutePath());
            return result;
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param vdb
     *        the VDB where the model is being created (cannot be <code>null</code>)
     * @param modelName
     *        the name of the model to create (cannot be empty)
     * @return the VDB import model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Model createModel( final UnitOfWork transaction,
                                     final Repository repository,
                                     final Vdb vdb,
                                     final String modelName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( vdb, "vdb" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( modelName, "modelName" ); //$NON-NLS-1$

        final KomodoObject kobject = vdb.addChild( transaction, modelName, VdbLexicon.Vdb.DECLARATIVE_MODEL );
        final Model result = new ModelImpl( transaction, repository, kobject.getAbsolutePath() );

        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentModel
     *        the model where the model source is being created (cannot be <code>null</code>)
     * @param sourceName
     *        the name of the model source to create (cannot be empty)
     * @return the model source object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static ModelSource createModelSource( final UnitOfWork transaction,
                                                 final Repository repository,
                                                 final Model parentModel,
                                                 final String sourceName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentModel, "parentModel"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(sourceName, "sourceName"); //$NON-NLS-1$

        try {
            final KomodoObject grouping = RepositoryTools.findOrCreateChild(transaction,
                                                                            parentModel,
                                                                            VdbLexicon.Vdb.SOURCES,
                                                                            VdbLexicon.Vdb.SOURCES);
            final KomodoObject kobject = grouping.addChild(transaction, sourceName, VdbLexicon.Source.SOURCE);
            final ModelSource result = new ModelSourceImpl(transaction, repository, kobject.getAbsolutePath());
            return result;
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
    public static Parameter createParameter( final UnitOfWork transaction,
                                             final Repository repository,
                                             final AbstractProcedure parentProcedure,
                                             final String parameterName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentProcedure, "parentProcedure"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(parameterName, "parameterName"); //$NON-NLS-1$

        try {
            final KomodoObject kobject = repository.add(transaction, parentProcedure.getAbsolutePath(), parameterName, null);
            kobject.addDescriptor(transaction, CreateProcedure.PARAMETER);

            final Parameter result = new ParameterImpl(transaction, repository, kobject.getAbsolutePath());
            return result;
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
    public static Permission createPermission( final UnitOfWork transaction,
                                               final Repository repository,
                                               final DataRole parentDataRole,
                                               final String permissionName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentDataRole, "parentDataRole"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(permissionName, "permissionName"); //$NON-NLS-1$

        try {
            final KomodoObject grouping = RepositoryTools.findOrCreateChild(transaction,
                                                                            parentDataRole,
                                                                            VdbLexicon.DataRole.PERMISSIONS,
                                                                            VdbLexicon.DataRole.PERMISSIONS);
            final KomodoObject kobject = grouping.addChild(transaction, permissionName, VdbLexicon.DataRole.Permission.PERMISSION);
            final Permission result = new PermissionImpl(transaction, repository, kobject.getAbsolutePath());
            return result;
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
    public static PrimaryKey createPrimaryKey( final UnitOfWork transaction,
                                               final Repository repository,
                                               final Table parentTable,
                                               final String primaryKeyName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( parentTable, "parentTable" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( primaryKeyName, "primaryKeyName" ); //$NON-NLS-1$

        final KomodoObject kobject = repository.add( transaction, parentTable.getAbsolutePath(), primaryKeyName, null );
        kobject.addDescriptor( transaction, Constraint.TABLE_ELEMENT );
        kobject.setProperty( transaction, Constraint.TYPE, PrimaryKey.CONSTRAINT_TYPE.toValue() );

        final PrimaryKey result = new PrimaryKeyImpl( transaction, repository, kobject.getAbsolutePath() );
        return result;
    }

    /**
     * This is Teiid's <code>Create Foreign Procedure</code> command.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
    public static StoredProcedure createStoredProcedure( final UnitOfWork transaction,
                                                         final Repository repository,
                                                         final Model parentModel,
                                                         final String procedureName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( parentModel, "parentModel" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( procedureName, "procedureName" ); //$NON-NLS-1$

        final KomodoObject kobject = repository.add( transaction, parentModel.getAbsolutePath(), procedureName, null );
        kobject.addDescriptor( transaction, CreateProcedure.PROCEDURE_STATEMENT );
        kobject.setProperty( transaction, SchemaElement.TYPE, SchemaElementType.FOREIGN.name() );
        setCreateStatementProperties( transaction, kobject );

        final StoredProcedure result = new StoredProcedureImpl( transaction, repository, kobject.getAbsolutePath() );
        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param resultSet
     *        the tabular result set where the column is being created (cannot be <code>null</code>)
     * @param columnName
     *        the name of the column to create (cannot be empty)
     * @return the result set column model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static ResultSetColumn createResultSetColumn( final UnitOfWork transaction,
                                                         final Repository repository,
                                                         final TabularResultSet resultSet,
                                                         final String columnName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( resultSet, "resultSet" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( columnName, "columnName" ); //$NON-NLS-1$

        final KomodoObject kobject = repository.add( transaction, resultSet.getAbsolutePath(), columnName, null );
        kobject.addDescriptor( transaction, CreateProcedure.RESULT_COLUMN );

        final ResultSetColumn result = new ResultSetColumnImpl( transaction, repository, kobject.getAbsolutePath() );
        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentWorkspacePath
     *        the parent path (can be empty)
     * @param schemaName
     *        the name of the schema fragment to create (cannot be empty)
     * @return the Schema model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Schema createSchema( final UnitOfWork transaction,
                                 final Repository repository,
                                 final String parentWorkspacePath,
                                       final String schemaName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( schemaName, "schemaName" ); //$NON-NLS-1$

        // make sure path is in the library
        String parentPath = parentWorkspacePath;
        final String workspacePath = repository.komodoWorkspace( transaction ).getAbsolutePath();

        if ( StringUtils.isBlank( parentWorkspacePath ) ) {
            parentPath = workspacePath;
        } else if ( !parentPath.startsWith( workspacePath ) ) {
            parentPath = ( workspacePath + parentPath );
        }

        final KomodoObject kobject = repository.add( transaction, parentPath, schemaName, KomodoLexicon.Schema.NODE_TYPE );
        final Schema result = new SchemaImpl( transaction, repository, kobject.getAbsolutePath() );
        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
    public static StatementOption createStatementOption( final UnitOfWork transaction,
                                                         final Repository repository,
                                                         final OptionContainer optionContainer,
                                                         final String optionName,
                                                         final String optionValue ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( optionContainer, "optionContainer" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( optionName, "optionName" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( optionValue, "optionValue" ); //$NON-NLS-1$

        final KomodoObject kobject = repository.add( transaction, optionContainer.getAbsolutePath(), optionName, null );
        kobject.addDescriptor( transaction, StandardDdlLexicon.TYPE_STATEMENT_OPTION );

        final StatementOption result = new StatementOptionImpl( transaction, repository, kobject.getAbsolutePath() );
        result.setOption( transaction, optionValue );
        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
    public static Table createTable( final UnitOfWork transaction,
                                     final Repository repository,
                                     final Model parentModel,
                                     final String tableName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( parentModel, "parentModel" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( tableName, "tableName" ); //$NON-NLS-1$

        final KomodoObject kobject = repository.add( transaction, parentModel.getAbsolutePath(), tableName, null );
        kobject.addDescriptor( transaction, CreateTable.TABLE_STATEMENT );
        setCreateStatementProperties( transaction, kobject );

        final Table result = new TableImpl( transaction, repository, kobject.getAbsolutePath() );
        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentProcedure
     *        the procedure where the procedure result set model object is being created (cannot be <code>null</code>)
     * @return the procedure result set model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static TabularResultSet createTabularResultSet( final UnitOfWork transaction,
                                                           final Repository repository,
                                                           final AbstractProcedure parentProcedure ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( parentProcedure, "parentProcedure" ); //$NON-NLS-1$

        final KomodoObject kobject = repository.add( transaction,
                                                     parentProcedure.getAbsolutePath(),
                                                     CreateProcedure.RESULT_SET,
                                                     null );
        kobject.addDescriptor( transaction, CreateProcedure.RESULT_COLUMNS );
        final TabularResultSet result = new TabularResultSetImpl( transaction, repository, kobject.getAbsolutePath() );
        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentWorkspacePath
     *        the parent path (can be empty)
     * @param id
     *        the name of the schema fragment to create (cannot be empty)
     * @return the Teiid model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Teiid createTeiid( final UnitOfWork transaction,
                                     final Repository repository,
                                     final String parentWorkspacePath,
                                     final String id ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( id, "id" ); //$NON-NLS-1$

        // make sure path is in the library
        String parentPath = parentWorkspacePath;
        final String workspacePath = repository.komodoWorkspace( transaction ).getAbsolutePath();

        if ( StringUtils.isBlank( parentWorkspacePath ) ) {
            parentPath = workspacePath;
        } else if ( !parentPath.startsWith( workspacePath ) ) {
            parentPath = ( workspacePath + parentPath );
        }

        final KomodoObject kobject = repository.add( transaction, parentPath, id, KomodoLexicon.Teiid.NODE_TYPE );
        final Teiid result = new TeiidImpl( transaction, repository, kobject.getAbsolutePath() );
        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param srcTeiid
     *        the source teiid model used for this caching teiid object (cannot be <code>null</code>)
     * @return the cached Teiid model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static CachedTeiid createCachedTeiid( final UnitOfWork transaction,
                                     final Repository repository,
                                     final Teiid srcTeiid ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( srcTeiid, "srcTeiid" ); //$NON-NLS-1$

        KomodoObject teiidCache = repository.komodoTeiidCache( transaction );
        final String parentPath = teiidCache.getAbsolutePath();
        final String id = srcTeiid.getName(transaction);

        //
        // Remove the existing version since its most likely out-of-date
        //
        if (teiidCache.hasChild(transaction, id))
            teiidCache.removeChild(transaction, id);

        final KomodoObject kobject = repository.add( transaction, parentPath, id, KomodoLexicon.CachedTeiid.NODE_TYPE );

        //
        // Populates the node with the source teiid's properties
        //
        final CachedTeiid result = new CachedTeiidImpl( transaction, srcTeiid, kobject.getAbsolutePath() );
        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
    public static Translator createTranslator( final UnitOfWork transaction,
                                               final Repository repository,
                                               final Vdb parentVdb,
                                               final String translatorName,
                                               final String translatorType ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentVdb, "parentVdb"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(translatorName, "translatorName"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(translatorType, "translatorType"); //$NON-NLS-1$

        try {
            final KomodoObject grouping = RepositoryTools.findOrCreateChild(transaction,
                                                                            parentVdb,
                                                                            VdbLexicon.Vdb.TRANSLATORS,
                                                                            VdbLexicon.Vdb.TRANSLATORS);
            final KomodoObject kobject = grouping.addChild(transaction, translatorName, VdbLexicon.Translator.TRANSLATOR);
            final Translator result = new TranslatorImpl(transaction, repository, kobject.getAbsolutePath());
            result.setType(transaction, translatorType);
            return result;
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
    public static UniqueConstraint createUniqueConstraint( final UnitOfWork transaction,
                                                           final Repository repository,
                                                           final Table parentTable,
                                                           final String uniqueConstraintName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( parentTable, "parentTable" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( uniqueConstraintName, "uniqueConstraintName" ); //$NON-NLS-1$

        final KomodoObject kobject = repository.add( transaction, parentTable.getAbsolutePath(), uniqueConstraintName, null );
        kobject.addDescriptor( transaction, Constraint.TABLE_ELEMENT );
        kobject.setProperty( transaction, Constraint.TYPE, UniqueConstraint.CONSTRAINT_TYPE.toValue() );

        final UniqueConstraint result = new UniqueConstraintImpl( transaction, repository, kobject.getAbsolutePath() );
        return result;
    }

    /**
     * This is Teiid's <code>Create Virtual Function</code> command.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
    public static UserDefinedFunction createUserDefinedFunction( final UnitOfWork transaction,
                                                                 final Repository repository,
                                                                 final Model parentModel,
                                                                 final String functionName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( parentModel, "parentModel" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( functionName, "functionName" ); //$NON-NLS-1$

        final KomodoObject kobject = repository.add( transaction, parentModel.getAbsolutePath(), functionName, null );
        kobject.addDescriptor( transaction, CreateProcedure.FUNCTION_STATEMENT );
        kobject.setProperty( transaction, SchemaElement.TYPE, SchemaElementType.VIRTUAL.name() );
        setCreateStatementProperties( transaction, kobject );

        final UserDefinedFunction result = new UserDefinedFunctionImpl( transaction, repository, kobject.getAbsolutePath() );
        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
    public static Vdb createVdb( final UnitOfWork transaction,
                                 final Repository repository,
                                 final String parentWorkspacePath,
                                 final String vdbName,
                                 final String externalFilePath ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( vdbName, "vdbName" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( externalFilePath, "externalFilePath" ); //$NON-NLS-1$

        // make sure path is in the library
        String parentPath = parentWorkspacePath;
        final String workspacePath = repository.komodoWorkspace( transaction ).getAbsolutePath();

        if ( StringUtils.isBlank( parentWorkspacePath ) ) {
            parentPath = workspacePath;
        } else if ( !parentPath.startsWith( workspacePath ) ) {
            parentPath = ( workspacePath + parentPath );
        }

        final KomodoObject kobject = repository.add( transaction, parentPath, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
        final Vdb result = new VdbImpl( transaction, repository, kobject.getAbsolutePath() );
        result.setOriginalFilePath( transaction, externalFilePath );
        result.setVdbName( transaction, vdbName );
        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
    public static VdbImport createVdbImport( final UnitOfWork transaction,
                                             final Repository repository,
                                             final Vdb parentVdb,
                                             final String vdbName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
        ArgCheck.isNotNull(parentVdb, "parentVdb"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(vdbName, "vdbName"); //$NON-NLS-1$

        try {
            final KomodoObject grouping = RepositoryTools.findOrCreateChild(transaction,
                                                                            parentVdb,
                                                                            VdbLexicon.Vdb.IMPORT_VDBS,
                                                                            VdbLexicon.Vdb.IMPORT_VDBS);
            final KomodoObject kobject = grouping.addChild(transaction, vdbName, VdbLexicon.ImportVdb.IMPORT_VDB);
            final VdbImport result = new VdbImportImpl(transaction, repository, kobject.getAbsolutePath());
            result.setVersion(transaction, Vdb.DEFAULT_VERSION);
            return result;
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
    public static View createView( final UnitOfWork transaction,
                                   final Repository repository,
                                   final Model parentModel,
                                   final String viewName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( parentModel, "parentModel" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( viewName, "viewName" ); //$NON-NLS-1$

        final KomodoObject kobject = repository.add( transaction, parentModel.getAbsolutePath(), viewName, null );
        kobject.addDescriptor( transaction, CreateTable.VIEW_STATEMENT );
        setCreateStatementProperties( transaction, kobject );

        final View result = new ViewImpl( transaction, repository, kobject.getAbsolutePath() );
        return result;
    }

    /**
     * This is Teiid's <code>Create Virtual Procedure</code> command.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
    public static VirtualProcedure createVirtualProcedure( final UnitOfWork transaction,
                                                           final Repository repository,
                                                           final Model parentModel,
                                                           final String procedureName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( parentModel, "parentModel" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( procedureName, "procedureName" ); //$NON-NLS-1$

        final KomodoObject kobject = repository.add( transaction, parentModel.getAbsolutePath(), procedureName, null );
        kobject.addDescriptor( transaction, CreateProcedure.PROCEDURE_STATEMENT );
        kobject.setProperty( transaction, SchemaElement.TYPE, SchemaElementType.VIRTUAL.name() );
        setCreateStatementProperties( transaction, kobject );

        final VirtualProcedure result = new VirtualProcedureImpl( transaction, repository, kobject.getAbsolutePath() );
        return result;
    }

    private static KException handleError( final Exception e ) {
        assert ( e != null );

        if ( e instanceof KException ) {
            return ( KException )e;
        }

        return new KException( e );
    }

    private static void setCreateStatementProperties( final UnitOfWork transaction,
                                                      final KomodoObject kobject ) throws KException {
        assert ( transaction != null );

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
