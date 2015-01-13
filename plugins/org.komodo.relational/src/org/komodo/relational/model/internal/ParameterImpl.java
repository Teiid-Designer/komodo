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
import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalConstants.Direction;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.Procedure;
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

/**
 * An implementation of a relational model procedure parameter.
 */
public final class ParameterImpl extends RelationalObjectImpl implements Parameter {

    /*

    - teiidddl:parameterType (string) = 'IN' mandatory autocreated < 'IN', 'OUT', 'INOUT', 'VARIADIC'
    TODO - teiidddl:result (boolean) = 'false' autocreated
    - ddl:nullable (string) = 'NULL' mandatory autocreated < 'NULL', 'NOT NULL'
    - ddl:datatypeName (STRING)
    - ddl:datatypeLength (LONG)
    - ddl:datatypePrecision (LONG)
    - ddl:datatypeScale (LONG)
    TODO - ddl:defaultOption (STRING) < 'LITERAL', 'DATETIME', 'USER', 'CURRENT_USER', 'SESSION_USER', 'SYSTEM_USER', 'NULL'
    - ddl:defaultValue (STRING)
    TODO - ddl:defaultPrecision (LONG)
    + * (ddl:statementOption) = ddl:statementOption

     */

    /**
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public ParameterImpl( final Repository repository,
                          final String workspacePath ) throws KException {
        super(repository, workspacePath);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Parameter#addStatementOption(org.komodo.spi.repository.Repository.UnitOfWork,
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
            transaction = getRepository().createTransaction("parameterimpl-addStatementOption", false, null); //$NON-NLS-1$
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
     * @see org.komodo.relational.model.Parameter#getDatatypeName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDatatypeName( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("parameterImpl-getDatatypeName", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            String result = RelationalConstants.DEFAULT_DATATYPE_NAME;
            final Property property = getProperty(transaction, StandardDdlLexicon.DATATYPE_NAME);

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
     * @see org.komodo.relational.model.Parameter#getDefaultValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDefaultValue( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("parameterImpl-getDefaultValue", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            String result = RelationalConstants.DEFAULT_VALUE;
            final Property property = getProperty(transaction, StandardDdlLexicon.DEFAULT_VALUE);

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
     * @see org.komodo.relational.model.Parameter#getDirection(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Direction getDirection( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("parameterImpl-getDirection", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            Direction result = Direction.DEFAULT_VALUE;
            final Property property = getProperty(transaction, CreateProcedure.PARAMETER_TYPE);

            if (property != null) {
                final String value = property.getStringValue();
                result = Direction.fromValue(value);
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
     * @see org.komodo.relational.model.Parameter#getLength(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public long getLength( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("parameterImpl-getLength", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            long result = RelationalConstants.DEFAULT_LENGTH;
            final Property property = getProperty(transaction, StandardDdlLexicon.DATATYPE_LENGTH);

            if (property != null) {
                result = property.getLongValue();
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
     * @see org.komodo.relational.model.Parameter#getNullable(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Nullable getNullable( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("parameterImpl-getNullable", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            Nullable result = Nullable.DEFAULT_VALUE;
            final Property property = getProperty(transaction, StandardDdlLexicon.NULLABLE);

            if (property != null) {
                final String value = property.getStringValue();
                result = Nullable.fromValue(value);
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
     * @see org.komodo.relational.model.Parameter#getPrecision(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public int getPrecision( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("parameterImpl-getPrecision", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            int result = RelationalConstants.DEFAULT_PRECISION;
            final Property property = getProperty(transaction, StandardDdlLexicon.DATATYPE_PRECISION);

            if (property != null) {
                result = (int)property.getLongValue();
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
     * @see org.komodo.relational.model.Parameter#getProcedure(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Procedure getProcedure( final UnitOfWork transaction ) throws KException {
        final KomodoObject kobject = getParent(transaction);
        return new ProcedureImpl(kobject.getRepository(), kobject.getAbsolutePath());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Parameter#getScale(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public int getScale( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("parameterImpl-getScale", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            int result = RelationalConstants.DEFAULT_SCALE;
            final Property property = getProperty(transaction, StandardDdlLexicon.DATATYPE_SCALE);

            if (property != null) {
                result = (int)property.getLongValue();
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
     * @see org.komodo.relational.model.Parameter#getStatementOptions(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public StatementOption[] getStatementOptions( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("parameterimpl-getStatementOptions", true, null); //$NON-NLS-1$
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
     * @see org.komodo.relational.model.Parameter#removeStatementOption(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void removeStatementOption( final UnitOfWork uow,
                                       final String optionToRemove ) throws KException {
        ArgCheck.isNotEmpty(optionToRemove, "optionToRemove"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("parameterimpl-removeStatementOption", false, null); //$NON-NLS-1$
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
     * @see org.komodo.relational.model.Parameter#setDatatypeName(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setDatatypeName( final UnitOfWork uow,
                                 final String newTypeName ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("parameterimpl-setDatatypeName", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setDatatypeName: transaction = '{0}', newTypeName = '{1}'", transaction.getName(), newTypeName); //$NON-NLS-1$
        }

        try {
            setProperty(transaction, StandardDdlLexicon.DATATYPE_NAME, StringUtils.isBlank(newTypeName) ? null : newTypeName);

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
     * @see org.komodo.relational.model.Parameter#setDefaultValue(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setDefaultValue( final UnitOfWork uow,
                                 final String newDefaultValue ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("parameterimpl-setDefaultValue", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setDefaultValue: transaction = '{0}', newDefaultValue = '{1}'", transaction.getName(), newDefaultValue); //$NON-NLS-1$
        }

        try {
            setProperty(transaction,
                        StandardDdlLexicon.DEFAULT_VALUE,
                        StringUtils.isBlank(newDefaultValue) ? null : newDefaultValue);

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
     * @see org.komodo.relational.model.Parameter#setDirection(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.RelationalConstants.Direction)
     */
    @Override
    public void setDirection( final UnitOfWork uow,
                              final Direction newDirection ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("parameterimpl-setDirection", false, null); //$NON-NLS-1$
        }

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setDirection: transaction = '{0}', newDirection = '{1}'", transaction.getName(), newDirection); //$NON-NLS-1$
        }

        try {
            setProperty(transaction,
                        CreateProcedure.PARAMETER_TYPE,
                        (newDirection == null) ? Direction.DEFAULT_VALUE : newDirection.toString());

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
     * @see org.komodo.relational.model.Parameter#setLength(org.komodo.spi.repository.Repository.UnitOfWork, long)
     */
    @Override
    public void setLength( final UnitOfWork uow,
                           final long newLength ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("parameterimpl-setLength", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setLength: transaction = '{0}', newLength = '{1}'", transaction.getName(), newLength); //$NON-NLS-1$
        }

        try {
            setProperty(transaction, StandardDdlLexicon.DATATYPE_LENGTH, newLength);

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
     * @see org.komodo.relational.model.Parameter#setNullable(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.RelationalConstants.Nullable)
     */
    @Override
    public void setNullable( final UnitOfWork uow,
                             final Nullable newNullable ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("parameterimpl-setNullable", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setNullable: transaction = '{0}', newNullable = '{1}'", transaction.getName(), newNullable); //$NON-NLS-1$
        }

        try {
            setProperty(transaction,
                        StandardDdlLexicon.NULLABLE,
                        (newNullable == null) ? Nullable.DEFAULT_VALUE : newNullable.toString());

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
     * @see org.komodo.relational.model.Parameter#setPrecision(org.komodo.spi.repository.Repository.UnitOfWork, int)
     */
    @Override
    public void setPrecision( final UnitOfWork uow,
                              final int newPrecision ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("parameterimpl-setPrecision", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setPrecision: transaction = '{0}', newPrecision = '{1}'", transaction.getName(), newPrecision); //$NON-NLS-1$
        }

        try {
            setProperty(transaction, StandardDdlLexicon.DATATYPE_PRECISION, newPrecision);

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
     * @see org.komodo.relational.model.Parameter#setScale(org.komodo.spi.repository.Repository.UnitOfWork, int)
     */
    @Override
    public void setScale( final UnitOfWork uow,
                          final int newScale ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("parameterimpl-setScale", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setScale: transaction = '{0}', newScale = '{1}'", transaction.getName(), newScale); //$NON-NLS-1$
        }

        try {
            setProperty(transaction, StandardDdlLexicon.DATATYPE_SCALE, newScale);

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

}
