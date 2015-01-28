/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.internal.model;

import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.ProcedureResultSet;
import org.komodo.relational.model.StatementOption;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.SchemaElement;

/**
 * An implementation of a relational model procedure.
 */
public final class ProcedureImpl extends RelationalObjectImpl implements Procedure {

    /*
      - teiidddl:schemaElementType (string) = 'FOREIGN' mandatory autocreated < 'FOREIGN', 'VIRTUAL'
      + * (teiidddl:procedureParameter) = teiidddl:procedureParameter
      + resultSet (teiidddl:resultSet)
      + * (ddl:statementOption) = ddl:statementOption
      - teiidddl:statement (string) -- procedure only, not function
     */

    /**
     * Creates a procedure.
     *
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public ProcedureImpl( final Repository repository,
                          final String workspacePath ) throws KException {
        super(repository, workspacePath);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Procedure#addParameter(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Parameter addParameter( final UnitOfWork uow,
                                   final String parameterName ) throws KException {
        ArgCheck.isNotEmpty(parameterName, "parameterName"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("procedureimpl-addParameter", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("addParameter: transaction = '{0}', parameterName = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         parameterName);
        }

        try {
            final Parameter result = RelationalModelFactory.createParameter(transaction, getRepository(), this, parameterName);

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
     * @see org.komodo.relational.model.OptionContainer#addStatementOption(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String, java.lang.String)
     */
    @Override
    public StatementOption addStatementOption( final UnitOfWork uow,
                                               final String optionName,
                                               final String optionValue ) throws KException {
        ArgCheck.isNotEmpty(optionName, "optionName"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(optionValue, "optionValue"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("procedureimpl-addStatementOption", false, null); //$NON-NLS-1$
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
                                                                                        this,
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
     * @see org.komodo.relational.model.Procedure#getAsClauseStatement(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getAsClauseStatement( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, Property.ValueType.STRING, "getAsClauseStatement", CreateProcedure.STATEMENT); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Procedure#getParameters(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Parameter[] getParameters( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("procedureImpl-getParameters", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject[] parameters = getChildrenOfType(transaction, CreateProcedure.PARAMETER);
            Parameter[] result = null;

            if (parameters.length == 0) {
                result = Parameter.NO_PARAMETERS;
            }

            result = new ParameterImpl[parameters.length];
            int i = 0;

            for (final KomodoObject param : parameters) {
                result[i] = new ParameterImpl(getRepository(), param.getAbsolutePath());
                ++i;
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
     * @see org.komodo.relational.model.Procedure#getResultSet(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public ProcedureResultSet getResultSet( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("procedureImpl-getResultSet", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject[] resultSets = getChildrenOfType(transaction, CreateProcedure.RESULT_SET);
            ProcedureResultSet result = null;

            // if does not exist create it
            if (resultSets.length == 0) {
                result = RelationalModelFactory.createProcedureResultSet(transaction, getRepository(), this);
            } else {
                result = new ProcedureResultSetImpl(getRepository(), resultSets[0].getAbsolutePath());
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
        final String value = getObjectProperty(uow,
                                               Property.ValueType.STRING,
                                               "getSchemaElementType", //$NON-NLS-1$
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
            transaction = getRepository().createTransaction("procedureimpl-getStatementOptions", true, null); //$NON-NLS-1$
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
     * @see org.komodo.relational.model.Procedure#isFunction(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isFunction( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("procedureImpl-isFunction", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final boolean result = hasDescriptor(transaction, CreateProcedure.FUNCTION_STATEMENT);

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
     * @see org.komodo.relational.model.Procedure#removeParameter(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void removeParameter( final UnitOfWork uow,
                                 final String parameterName ) throws KException {
        ArgCheck.isNotEmpty(parameterName, "parameterName"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("procedureimpl-removeParameter", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removeParameter: transaction = '{0}', parameterName = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         parameterName);
        }

        boolean found = false;

        try {
            for (final KomodoObject kobject : getChildrenOfType(transaction, CreateProcedure.PARAMETER)) {
                if (parameterName.equals(kobject.getName(transaction))) {
                    removeChild(transaction, parameterName);
                    found = true;
                    break;
                }
            }

            if (!found) {
                throw new KException(Messages.getString(Relational.PARAMETER_NOT_FOUND_TO_REMOVE, parameterName));
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
     * @see org.komodo.relational.model.OptionContainer#removeStatementOption(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeStatementOption( final UnitOfWork uow,
                                       final String optionToRemove ) throws KException {
        ArgCheck.isNotEmpty(optionToRemove, "optionToRemove"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("procedureimpl-removeStatementOption", false, null); //$NON-NLS-1$
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
     * @see org.komodo.relational.model.Procedure#setAsClauseStatement(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setAsClauseStatement( final UnitOfWork uow,
                                      final String newStatement ) throws KException {
        setObjectProperty(uow, "setAsClauseStatement", CreateProcedure.STATEMENT, newStatement); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Procedure#setFunction(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setFunction( final UnitOfWork uow,
                             final boolean newFunction ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("procedureimpl-setFunction", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setFunction: transaction = '{0}', newFunction = '{1}'", transaction.getName(), newFunction); //$NON-NLS-1$
        }

        try {
            if (newFunction != isFunction(transaction)) {
                if (newFunction) {
                    removeDescriptor(transaction, CreateProcedure.PROCEDURE_STATEMENT);
                    addDescriptor(transaction, CreateProcedure.FUNCTION_STATEMENT);
                } else {
                    removeDescriptor(transaction, CreateProcedure.FUNCTION_STATEMENT);
                    addDescriptor(transaction, CreateProcedure.PROCEDURE_STATEMENT);
                }
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
     * @see org.komodo.relational.model.SchemaElement#setSchemaElementType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.model.SchemaElement.SchemaElementType)
     */
    @Override
    public void setSchemaElementType( final UnitOfWork uow,
                                      final SchemaElementType newSchemaElementType ) throws KException {
        final String newValue = ((newSchemaElementType == null) ? SchemaElementType.DEFAULT_VALUE.toString()
                                                                : newSchemaElementType.toString());
        setObjectProperty(uow, "setSchemaElementType", SchemaElement.TYPE, newValue); //$NON-NLS-1$
    }

}
