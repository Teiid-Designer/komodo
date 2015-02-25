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
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.StatementOption;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
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
    - teiidddl:result (boolean) = 'false' autocreated
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
     * The resolver of a {@link Parameter}.
     */
    public static final TypeResolver RESOLVER = new TypeResolver() {

        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }

        @Override
        public Class<? extends KomodoObject> owningClass() {
            return ParameterImpl.class;
        }

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
                ObjectImpl.validateType(transaction, repository, kobject, CreateProcedure.PARAMETER);
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
        public Parameter resolve( final UnitOfWork transaction,
                                  final Repository repository,
                                  final KomodoObject kobject ) throws KException {
            return new ParameterImpl(transaction, repository, kobject.getAbsolutePath());
        }

        @Override
        public Parameter create(UnitOfWork transaction,
                                                      KomodoObject parent,
                                                      String id,
                                                      RelationalProperties properties) throws KException {
            AdapterFactory adapter = new AdapterFactory(parent.getRepository());
            Procedure parentProc = adapter.adapt(transaction, parent, Procedure.class);
            return RelationalModelFactory.createParameter(transaction, parent.getRepository(), parentProc, id);
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
     *         if an error occurs or if node at specified path is not a parameter
     */
    public ParameterImpl( final UnitOfWork uow,
                          final Repository repository,
                          final String workspacePath ) throws KException {
        super(uow, repository, workspacePath);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return RESOLVER.identifier();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#getCustomOptions(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public StatementOption[] getCustomOptions( final UnitOfWork transaction ) {
        return StatementOption.NO_OPTIONS;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Parameter#getDatatypeName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDatatypeName( final UnitOfWork uow ) throws KException {
        final String value = getObjectProperty(uow, Property.ValueType.STRING, "getDatatypeName", //$NON-NLS-1$
                                               StandardDdlLexicon.DATATYPE_NAME);

        if (StringUtils.isBlank(value)) {
            return RelationalConstants.DEFAULT_DATATYPE_NAME;
        }

        return value;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Parameter#getDefaultValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDefaultValue( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, Property.ValueType.STRING, "getDefaultValue", //$NON-NLS-1$
                                 StandardDdlLexicon.DEFAULT_VALUE);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Parameter#getDirection(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Direction getDirection( final UnitOfWork uow ) throws KException {
        final String value = getObjectProperty(uow, Property.ValueType.STRING, "getDirection", //$NON-NLS-1$
                                               CreateProcedure.PARAMETER_TYPE);

        if (StringUtils.isBlank(value)) {
            return Direction.DEFAULT_VALUE;
        }

        return Direction.fromValue(value);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Parameter#getLength(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public long getLength( final UnitOfWork uow ) throws KException {
        final Long value = getObjectProperty(uow, Property.ValueType.LONG, "getLength", //$NON-NLS-1$
                                             StandardDdlLexicon.DATATYPE_LENGTH);

        if (value == null) {
            return RelationalConstants.DEFAULT_LENGTH;
        }

        return value;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Parameter#getNullable(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Nullable getNullable( final UnitOfWork uow ) throws KException {
        final String value = getObjectProperty(uow, Property.ValueType.STRING, "getNullable", //$NON-NLS-1$
                                               StandardDdlLexicon.NULLABLE);

        if (StringUtils.isBlank(value)) {
            return Nullable.DEFAULT_VALUE;
        }

        return Nullable.fromValue(value);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Procedure getParent( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("parameterimpl-getParent", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject kobject = super.getParent(transaction);
            assert (kobject instanceof Procedure);
            final Procedure result = (Procedure)kobject;

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
        final Integer value = getObjectProperty(uow, Property.ValueType.INTEGER, "getPrecision", //$NON-NLS-1$
                                                StandardDdlLexicon.DATATYPE_PRECISION);

        if (value == null) {
            return RelationalConstants.DEFAULT_PRECISION;
        }

        return value;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Parameter#getProcedure(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Procedure getProcedure( final UnitOfWork transaction ) throws KException {
        return getParent(transaction);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Parameter#getScale(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public int getScale( final UnitOfWork uow ) throws KException {
        final Integer value = getObjectProperty(uow, Property.ValueType.INTEGER, "getScale", //$NON-NLS-1$
                                                StandardDdlLexicon.DATATYPE_SCALE);

        if (value == null) {
            return RelationalConstants.DEFAULT_SCALE;
        }

        return value;
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
            transaction = getRepository().createTransaction("parameterimpl-getStatementOptions", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("getStatementOptions: transaction = {0}", transaction.getName()); //$NON-NLS-1$
        }

        try {
            final List< StatementOption > result = new ArrayList< StatementOption >();

            for (final KomodoObject kobject : getChildrenOfType(transaction, StandardDdlLexicon.TYPE_STATEMENT_OPTION)) {
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
     * @see org.komodo.spi.repository.KomodoObject#getTypeId()
     */
    @Override
    public int getTypeId() {
        return TYPE_ID;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Parameter#isResult(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isResult( final UnitOfWork transaction ) throws KException {
        final Boolean value = getObjectProperty(transaction, Property.ValueType.BOOLEAN, "isResult", //$NON-NLS-1$
                                                CreateProcedure.PARAMETER_RESULT_FLAG);

        if (value == null) {
            return Parameter.DEFAULT_RESULT;
        }

        return value;
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
            transaction = getRepository().createTransaction("parameterimpl-removeStatementOption", false, null); //$NON-NLS-1$
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
     * @see org.komodo.relational.model.Parameter#setDatatypeName(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setDatatypeName( final UnitOfWork uow,
                                 final String newTypeName ) throws KException {
        setObjectProperty(uow, "setDatatypeName", StandardDdlLexicon.DATATYPE_NAME, newTypeName); //$NON-NLS-1$
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
        setObjectProperty(uow, "setDefaultValue", StandardDdlLexicon.DEFAULT_VALUE, newDefaultValue); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Parameter#setDirection(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.model.Parameter.Direction)
     */
    @Override
    public void setDirection( final UnitOfWork uow,
                              final Direction newDirection ) throws KException {
        setObjectProperty(uow, "setDirection", //$NON-NLS-1$
                          CreateProcedure.PARAMETER_TYPE,
                          (newDirection == null) ? Direction.DEFAULT_VALUE.toString() : newDirection.toString());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Parameter#setLength(org.komodo.spi.repository.Repository.UnitOfWork, long)
     */
    @Override
    public void setLength( final UnitOfWork uow,
                           final long newLength ) throws KException {
        setObjectProperty(uow, "setLength", StandardDdlLexicon.DATATYPE_LENGTH, newLength); //$NON-NLS-1$
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
        setObjectProperty(uow, "setNullable", //$NON-NLS-1$
                          StandardDdlLexicon.NULLABLE,
                          (newNullable == null) ? Nullable.DEFAULT_VALUE.toString() : newNullable.toString());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Parameter#setPrecision(org.komodo.spi.repository.Repository.UnitOfWork, int)
     */
    @Override
    public void setPrecision( final UnitOfWork uow,
                              final int newPrecision ) throws KException {
        setObjectProperty(uow, "setPrecision", StandardDdlLexicon.DATATYPE_PRECISION, newPrecision); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Parameter#setResult(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setResult( final UnitOfWork transaction,
                           final boolean newResult ) throws KException {
        setObjectProperty(transaction, "setResult", CreateProcedure.PARAMETER_RESULT_FLAG, newResult); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Parameter#setScale(org.komodo.spi.repository.Repository.UnitOfWork, int)
     */
    @Override
    public void setScale( final UnitOfWork uow,
                          final int newScale ) throws KException {
        setObjectProperty(uow, "setScale", StandardDdlLexicon.DATATYPE_SCALE, newScale); //$NON-NLS-1$
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
            transaction = getRepository().createTransaction("parameterimpl-setStatementOption", false, null); //$NON-NLS-1$
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
