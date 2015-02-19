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
import org.komodo.relational.model.AbstractProcedure;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Parameter;
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
abstract class AbstractProcedureImpl extends RelationalObjectImpl implements AbstractProcedure {

    /*
      - teiidddl:schemaElementType (string) = 'FOREIGN' mandatory autocreated < 'FOREIGN', 'VIRTUAL'
      + * (teiidddl:procedureParameter) = teiidddl:procedureParameter
      + resultSet (teiidddl:resultSet)
      + * (ddl:statementOption) = ddl:statementOption
      - teiidddl:statement (string) -- procedure only, not function
     */

    protected enum StandardOptions {

        ANNOTATION( "ANNOTATION" ), //$NON-NLS-1$
        NAMEINSOURCE( "NAMEINSOURCE" ), //$NON-NLS-1$
        NATIVE_QUERY( "native-query" ), //$NON-NLS-1$
        UUID( "UUID" ); //$NON-NLS-1$

        private final String name;

        private StandardOptions( final String optionName ) {
            this.name = optionName;
        }

        public String getName() {
            return this.name;
        }

    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs or if node at specified path is not a procedure
     */
    protected AbstractProcedureImpl( final UnitOfWork uow,
                                     final Repository repository,
                                     final String workspacePath ) throws KException {
        super(uow, repository, workspacePath);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.AbstractProcedure#addParameter(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public Parameter addParameter( final UnitOfWork uow,
                                   final String parameterName ) throws KException {
        ArgCheck.isNotEmpty(parameterName, "parameterName"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("abstractprocedureimpl-addParameter", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("addParameter: transaction = {0}, parameterName = {1}", //$NON-NLS-1$
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
     * @see org.komodo.relational.model.AbstractProcedure#getAsClauseStatement(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getAsClauseStatement( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, Property.ValueType.STRING, "getAsClauseStatement", CreateProcedure.STATEMENT); //$NON-NLS-1$
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
            transaction = getRepository().createTransaction("abstractprocedureimpl-getChildren", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject[] params = getParameters(transaction);
            final ProcedureResultSet resultSet = getResultSet(transaction, false);
            final KomodoObject[] result = new KomodoObject[params.length + ((resultSet == null) ? 0 : 1)];
            System.arraycopy(params, 0, result, 0, params.length);

            if (resultSet != null) {
                result[params.length] = resultSet;
            }

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }
//
//    /**
//     * {@inheritDoc}
//     *
//     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildrenOfType(org.komodo.spi.repository.Repository.UnitOfWork,
//     *      java.lang.String)
//     */
//    @Override
//    public KomodoObject[] getChildrenOfType( final UnitOfWork uow,
//                                             final String type ) throws KException {
//        UnitOfWork transaction = uow;
//
//        if (transaction == null) {
//            transaction = getRepository().createTransaction("abstractprocedureimpl-getChildrenOfType", true, null); //$NON-NLS-1$
//        }
//
//        assert (transaction != null);
//
//        try {
//            KomodoObject[] result = KomodoObject.EMPTY_ARRAY;
//
//            if (CreateProcedure.PARAMETER.equals(type)) {
//                result = getParameters(transaction);
//            } else if ((KomodoLexicon.TeiidDdl.RESULT_SET_NODE_TYPE.equals(type) || CreateProcedure.RESULT_COLUMNS.equals(type))
//                       || CreateProcedure.RESULT_DATA_TYPE.equals(type)) {
//                final ProcedureResultSet resultSet = getResultSet(transaction, false);
//
//                if (resultSet != null) {
//                    result = new KomodoObject[] {resultSet};
//                }
//            }
//
//            if (uow == null) {
//                transaction.commit();
//            }
//
//            return result;
//        } catch (final Exception e) {
//            throw handleError(uow, transaction, e);
//        }
//    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#getCustomOptions(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public StatementOption[] getCustomOptions( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("abstractprocedureimpl-getCustomOptions", true, null); //$NON-NLS-1$
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
     * @see org.komodo.relational.model.AbstractProcedure#getDescription(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDescription( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.ANNOTATION.getName());

        if (option == null) {
            return null;
        }

        return option.getOption(transaction);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.AbstractProcedure#getNameInSource(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getNameInSource( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.NAMEINSOURCE.getName());

        if (option == null) {
            return null;
        }

        return option.getOption(transaction);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Procedure#getNativeQuery(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getNativeQuery( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption(transaction, this, StandardOptions.NATIVE_QUERY.getName());

        if (option == null) {
            return null;
        }

        return option.getOption(transaction);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.AbstractProcedure#getParameters(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Parameter[] getParameters( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("procedureImpl-getParameters", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject[] parameters = super.getChildrenOfType(transaction, CreateProcedure.PARAMETER);
            Parameter[] result = null;

            if (parameters.length == 0) {
                result = Parameter.NO_PARAMETERS;
            }

            result = new ParameterImpl[parameters.length];
            int i = 0;

            for (final KomodoObject param : parameters) {
                result[i] = new ParameterImpl(transaction, getRepository(), param.getAbsolutePath());
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
     * @see org.komodo.repository.ObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Model getParent( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("procedureimpl-getParent", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject kobject = super.getParent(transaction);
            assert (kobject instanceof Model);
            final Model result = (Model)kobject;

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
     * @see org.komodo.relational.model.AbstractProcedure#getResultSet(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public ProcedureResultSet getResultSet( final UnitOfWork uow,
                                            final boolean create ) throws KException {
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = getRepository().createTransaction("procedureImpl-getResultSet", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            KomodoObject[] kids = getChildrenOfType(transaction, CreateProcedure.RESULT_COLUMNS);

            if (kids.length == 0) {
                kids = getChildrenOfType(transaction, CreateProcedure.RESULT_DATA_TYPE);
            }

            ProcedureResultSet result = null;

            if (kids.length == 0) {
                if (create) {
                    result = RelationalModelFactory.createProcedureResultSet(transaction, getRepository(), this);
                }
            } else {
                result = new ProcedureResultSetImpl(transaction, getRepository(), kids[0].getAbsolutePath());
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
            transaction = getRepository().createTransaction("abstractprocedureimpl-getStatementOptions", true, null); //$NON-NLS-1$
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
     * @see org.komodo.relational.model.AbstractProcedure#removeParameter(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void removeParameter( final UnitOfWork uow,
                                 final String parameterName ) throws KException {
        ArgCheck.isNotEmpty(parameterName, "parameterName"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("abstractprocedureimpl-removeParameter", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removeParameter: transaction = {0}, parameterName = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         parameterName);
        }

        boolean found = false;

        try {
            final Parameter[] parameters = getParameters(transaction);

            if (parameters.length != 0) {
                for (final Parameter parameter : parameters) {
                    if (parameterName.equals(parameter.getName(transaction))) {
                        removeChild(transaction, parameterName);
                        found = true;
                        break;
                    }
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
     * @see org.komodo.relational.model.AbstractProcedure#removeResultSet(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public void removeResultSet( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = getRepository().createTransaction("abstractprocedureimpl-removeResultSet", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removeResultSet: transaction = {0}", transaction.getName()); //$NON-NLS-1$
        }

        try {
            final ProcedureResultSet resultSet = getResultSet(transaction, false);

            if (resultSet == null) {
                throw new KException(Messages.getString(Relational.RESULT_SET_NOT_FOUND_TO_REMOVE, getName(transaction)));
            }

            removeChild(transaction, CreateProcedure.RESULT_SET);

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
            transaction = getRepository().createTransaction("abstractprocedureimpl-removeStatementOption", false, null); //$NON-NLS-1$
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
     * @see org.komodo.relational.model.AbstractProcedure#setAsClauseStatement(org.komodo.spi.repository.Repository.UnitOfWork,
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
     * @see org.komodo.relational.model.AbstractProcedure#setDescription(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setDescription( final UnitOfWork transaction,
                                final String newDescription ) throws KException {
        setStatementOption(transaction, StandardOptions.ANNOTATION.getName(), newDescription);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.AbstractProcedure#setNameInSource(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setNameInSource( final UnitOfWork transaction,
                                 final String newNameInSource ) throws KException {
        setStatementOption(transaction, StandardOptions.NAMEINSOURCE.getName(), newNameInSource);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Procedure#setNativeQuery(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setNativeQuery( final UnitOfWork transaction,
                                final String newNativeQuery ) throws KException {
        setStatementOption(transaction, StandardOptions.NATIVE_QUERY.getName(), newNativeQuery);
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
            transaction = getRepository().createTransaction("abstractprocedureimpl-setStatementOption", false, null); //$NON-NLS-1$
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

}
