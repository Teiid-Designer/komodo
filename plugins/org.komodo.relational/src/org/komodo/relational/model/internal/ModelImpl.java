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
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.RelationalModelFactory;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.View;
import org.komodo.spi.KException;
import org.komodo.spi.Messages;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateTable;
import org.modeshape.sequencer.teiid.lexicon.CoreLexicon;

/**
 * An implementation of a relational model.
 */
public final class ModelImpl extends RelationalObjectImpl implements Model {

    /**
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public ModelImpl( final Repository repository,
                      final String workspacePath ) throws KException {
        super(repository, workspacePath);
    }

    @Override
    public String getModelType( final UnitOfWork uow ) throws KException {
        String modelType = getObjectProperty(uow,
                                       Property.ValueType.STRING,
                                       "getModelType", //$NON-NLS-1$
                                       CoreLexicon.JcrId.MODEL_TYPE);
        
        return modelType == null ? EMPTY_STRING : modelType;
    }

    @Override
    public void setModelType(UnitOfWork uow, String modelType) throws KException {
        setObjectProperty(uow, "setModelType", CoreLexicon.JcrId.MODEL_TYPE, modelType); //$NON-NLS-1$
    }

    @Override
    public String getModelDefinition(UnitOfWork uow) throws KException {
        String modelDefn = getObjectProperty(uow,
                                       Property.ValueType.STRING,
                                       "getModelDefinition", //$NON-NLS-1$
                                       KomodoLexicon.VdbModel.MODEL_DEFINITION);
        
        return modelDefn == null ? EMPTY_STRING : modelDefn;
    }
    
    @Override
    public void setModelDefinition(UnitOfWork uow, String modelDefinition) throws KException {
        setObjectProperty(uow, "setModelDefinition", KomodoLexicon.VdbModel.MODEL_DEFINITION, modelDefinition); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#addFunction(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Procedure addFunction( final UnitOfWork uow,
                                  final String functionName ) throws KException {
        ArgCheck.isNotEmpty(functionName, "functionName"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("modelimpl-addFunction", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("addFunction: transaction = '{0}', functionName = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         functionName);
        }

        try {
            final Procedure result = addProcedure(transaction, functionName);
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
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#addProcedure(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Procedure addProcedure( final UnitOfWork uow,
                                   final String procedureName ) throws KException {
        ArgCheck.isNotEmpty(procedureName, "procedureName"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("modelimpl-addProcedure", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("addProcedure: transaction = '{0}', procedureName = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         procedureName);
        }

        try {
            final Procedure result = RelationalModelFactory.createProcedure(transaction, getRepository(), getAbsolutePath(), procedureName);

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
     * @see org.komodo.relational.model.Model#addTable(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Table addTable( final UnitOfWork uow,
                           final String tableName ) throws KException {
        ArgCheck.isNotEmpty(tableName, "tableName"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("modelimpl-addTable", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("addTable: transaction = '{0}', tableName = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         tableName);
        }

        try {
            final Table result = RelationalModelFactory.createTable(transaction, getRepository(), getAbsolutePath(), tableName);

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
     * @see org.komodo.relational.model.Model#addView(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public View addView( final UnitOfWork uow,
                         final String viewName ) throws KException {
        ArgCheck.isNotEmpty(viewName, "viewName"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("modelimpl-viewName", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("addView: transaction = '{0}', viewName = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         viewName);
        }

        try {
            final View result = RelationalModelFactory.createView(transaction, getRepository(), getAbsolutePath(), viewName);

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
     * @see org.komodo.relational.model.Model#getFunctions(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Procedure[] getFunctions( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("modelimpl-getFunctions", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("getFunctions: transaction = '{0}'", transaction.getName()); //$NON-NLS-1$
        }

        try {
            final List< Procedure > result = new ArrayList< Procedure >();

            for (final KomodoObject kobject : getChildrenOfType(transaction, TeiidDdlLexicon.CreateProcedure.FUNCTION_STATEMENT)) {
                final Procedure procedure = new ProcedureImpl(getRepository(), kobject.getAbsolutePath());

                if (LOGGER.isDebugEnabled()) {
                    LOGGER.debug("getFunctions: transaction = '{0}', found function = '{1}'", //$NON-NLS-1$
                                 transaction.getName(),
                                 kobject.getAbsolutePath());
                }

                result.add(procedure);
            }

            if (uow == null) {
                transaction.commit();
            }

            if (result.isEmpty()) {
                return Procedure.NO_PROCEDURES;
            }

            return result.toArray(new Procedure[result.size()]);
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#getProcedures(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Procedure[] getProcedures( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("modelimpl-getProcedures", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("getProcedures: transaction = '{0}'", transaction.getName()); //$NON-NLS-1$
        }

        try {
            final List< Procedure > result = new ArrayList< Procedure >();

            for (final KomodoObject kobject : getChildrenOfType(transaction, TeiidDdlLexicon.CreateProcedure.PROCEDURE_STATEMENT)) {
                final Procedure procedure = new ProcedureImpl(getRepository(), kobject.getAbsolutePath());

                if (LOGGER.isDebugEnabled()) {
                    LOGGER.debug("getProcedures: transaction = '{0}', found procedure = '{1}'", //$NON-NLS-1$
                                 transaction.getName(),
                                 kobject.getAbsolutePath());
                }

                result.add(procedure);
            }

            if (uow == null) {
                transaction.commit();
            }

            if (result.isEmpty()) {
                return Procedure.NO_PROCEDURES;
            }

            return result.toArray(new Procedure[result.size()]);
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#getTables(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Table[] getTables( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("modelimpl-getTables", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("getTables: transaction = '{0}'", transaction.getName()); //$NON-NLS-1$
        }

        try {
            final List< Table > result = new ArrayList< Table >();

            for (final KomodoObject kobject : getChildrenOfType(transaction, TeiidDdlLexicon.CreateTable.TABLE_STATEMENT)) {
                final Table table = new TableImpl(getRepository(), kobject.getAbsolutePath());

                if (LOGGER.isDebugEnabled()) {
                    LOGGER.debug("getTables: transaction = '{0}', found procedure = '{1}'", //$NON-NLS-1$
                                 transaction.getName(),
                                 kobject.getAbsolutePath());
                }

                result.add(table);
            }

            if (uow == null) {
                transaction.commit();
            }

            if (result.isEmpty()) {
                return Table.NO_TABLES;
            }

            return result.toArray(new Table[result.size()]);
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#getViews(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public View[] getViews( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("modelimpl-getViews", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("getViews: transaction = '{0}'", transaction.getName()); //$NON-NLS-1$
        }

        try {
            final List< View > result = new ArrayList< View >();

            for (final KomodoObject kobject : getChildrenOfType(transaction, TeiidDdlLexicon.CreateTable.VIEW_STATEMENT)) {
                final View view = new ViewImpl(getRepository(), kobject.getAbsolutePath());

                if (LOGGER.isDebugEnabled()) {
                    LOGGER.debug("getViews: transaction = '{0}', found view = '{1}'", //$NON-NLS-1$
                                 transaction.getName(),
                                 kobject.getAbsolutePath());
                }

                result.add(view);
            }

            if (uow == null) {
                transaction.commit();
            }

            if (result.isEmpty()) {
                return View.NO_VIEWS;
            }

            return result.toArray(new View[result.size()]);
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#removeFunction(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeFunction( final UnitOfWork uow,
                                final String functionName ) throws KException {
        ArgCheck.isNotEmpty(functionName, "functionName"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("modelimpl-removeFunction", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removeFunction: transaction = '{0}', functionName = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         functionName);
        }

        boolean found = false;

        try {
            for (final KomodoObject kobject : getChildrenOfType(transaction, CreateProcedure.FUNCTION_STATEMENT)) {
                if (functionName.equals(kobject.getName(transaction))) {
                    removeChild(transaction, functionName);
                    found = true;
                    break;
                }
            }

            if (!found) {
                throw new KException(Messages.getString(Relational.FUNCTION_NOT_FOUND_TO_REMOVE, functionName));
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
     * @see org.komodo.relational.model.Model#removeProcedure(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeProcedure( final UnitOfWork uow,
                                 final String procedureName ) throws KException {
        ArgCheck.isNotEmpty(procedureName, "procedureName"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("modelimpl-removeProcedure", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removeProcedure: transaction = '{0}', procedureName = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         procedureName);
        }

        boolean found = false;

        try {
            for (final KomodoObject kobject : getChildrenOfType(transaction, CreateProcedure.PROCEDURE_STATEMENT)) {
                if (procedureName.equals(kobject.getName(transaction))) {
                    removeChild(transaction, procedureName);
                    found = true;
                    break;
                }
            }

            if (!found) {
                throw new KException(Messages.getString(Relational.PROCEDURE_NOT_FOUND_TO_REMOVE, procedureName));
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
     * @see org.komodo.relational.model.Model#removeTable(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeTable( final UnitOfWork uow,
                             final String tableName ) throws KException {
        ArgCheck.isNotEmpty(tableName, "tableName"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("modelimpl-removeTable", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removeTable: transaction = '{0}', tableName = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         tableName);
        }

        boolean found = false;

        try {
            for (final KomodoObject kobject : getChildrenOfType(transaction, CreateTable.TABLE_STATEMENT)) {
                if (tableName.equals(kobject.getName(transaction))) {
                    removeChild(transaction, tableName);
                    found = true;
                    break;
                }
            }

            if (!found) {
                throw new KException(Messages.getString(Relational.TABLE_NOT_FOUND_TO_REMOVE, tableName));
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
     * @see org.komodo.relational.model.Model#removeView(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeView( final UnitOfWork uow,
                            final String viewName ) throws KException {
        ArgCheck.isNotEmpty(viewName, "viewName"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("modelimpl-removeView", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removeView: transaction = '{0}', viewName = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         viewName);
        }

        boolean found = false;

        try {
            for (final KomodoObject kobject : getChildrenOfType(transaction, CreateTable.VIEW_STATEMENT)) {
                if (viewName.equals(kobject.getName(transaction))) {
                    removeChild(transaction, viewName);
                    found = true;
                    break;
                }
            }

            if (!found) {
                throw new KException(Messages.getString(Relational.VIEW_NOT_FOUND_TO_REMOVE, viewName));
            }

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

}
