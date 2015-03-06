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
import org.komodo.relational.model.PushdownFunction;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.relational.model.UserDefinedFunction;
import org.komodo.relational.model.VirtualProcedure;
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
 * A base implementation of a relational model procedure or function.
 */
abstract class AbstractProcedureImpl extends RelationalObjectImpl implements AbstractProcedure {

    static Class< ? extends AbstractProcedure > getProcedureType( final UnitOfWork transaction,
                                                                  final KomodoObject kobject ) throws KException {
        Class< ? extends AbstractProcedure > clazz = null;

        if (PushdownFunctionImpl.RESOLVER.resolvable( transaction, kobject )) {
            clazz = PushdownFunction.class;
        } else if (UserDefinedFunctionImpl.RESOLVER.resolvable( transaction, kobject )) {
            clazz = UserDefinedFunction.class;
        } else if (StoredProcedureImpl.RESOLVER.resolvable( transaction, kobject )) {
            clazz = StoredProcedure.class;
        } else if (VirtualProcedureImpl.RESOLVER.resolvable( transaction, kobject )) {
            clazz = VirtualProcedure.class;
        } else {
            throw new KException( Messages.getString( Relational.UNEXPECTED_PROCEDURE_TYPE, kobject.getAbsolutePath() ) );
        }

        return clazz;
    }

    protected enum StandardOptions {

        ANNOTATION,
        NAMEINSOURCE,
        UPDATECOUNT,
        UUID;

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
        super( uow, repository, workspacePath );
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
        ArgCheck.isNotEmpty( parameterName, "parameterName" ); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction( "abstractprocedureimpl-addParameter", false, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "addParameter: transaction = {0}, parameterName = {1}", //$NON-NLS-1$
                          transaction.getName(),
                          parameterName );
        }

        try {
            final Parameter result = RelationalModelFactory.createParameter( transaction, getRepository(), this, parameterName );

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError( uow, transaction, e );
        }
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
            transaction = getRepository().createTransaction( "abstractprocedureimpl-getChildren", true, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        try {
            final KomodoObject[] result = getParameters( transaction );

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError( uow, transaction, e );
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
            transaction = getRepository().createTransaction( "abstractprocedureimpl-getCustomOptions", true, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        try {
            StatementOption[] result = getStatementOptions( transaction );

            if (result.length != 0) {
                final List< StatementOption > temp = new ArrayList<>( result.length );

                for (final StatementOption option : result) {
                    if (StandardOptions.valueOf( option.getName( transaction ) ) == null) {
                        temp.add( option );
                    }
                }

                result = temp.toArray( new StatementOption[temp.size()] );
            }

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.AbstractProcedure#getDescription(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDescription( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption( transaction, this, StandardOptions.ANNOTATION.name() );

        if (option == null) {
            return null;
        }

        return option.getOption( transaction );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.AbstractProcedure#getNameInSource(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getNameInSource( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption( transaction, this, StandardOptions.NAMEINSOURCE.name() );

        if (option == null) {
            return null;
        }

        return option.getOption( transaction );
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
            transaction = getRepository().createTransaction( "procedureImpl-getParameters", true, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        try {
            final KomodoObject[] parameters = super.getChildrenOfType( transaction, CreateProcedure.PARAMETER );
            Parameter[] result = null;

            if (parameters.length == 0) {
                result = Parameter.NO_PARAMETERS;
            }

            result = new ParameterImpl[parameters.length];
            int i = 0;

            for (final KomodoObject param : parameters) {
                result[i] = new ParameterImpl( transaction, getRepository(), param.getAbsolutePath() );
                ++i;
            }

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError( uow, transaction, e );
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
            transaction = getRepository().createTransaction( "procedureimpl-getParent", true, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        try {
            final KomodoObject kobject = super.getParent( transaction );
            assert ( kobject instanceof Model );
            final Model result = ( Model )kobject;

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.SchemaElement#getSchemaElementType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public SchemaElementType getSchemaElementType( final UnitOfWork uow ) throws KException {
        final String value = getObjectProperty( uow, Property.ValueType.STRING, "getSchemaElementType", //$NON-NLS-1$
                                                SchemaElement.TYPE );

        if (StringUtils.isBlank( value )) {
            return null;
        }

        return SchemaElementType.fromValue( value );
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
            transaction = getRepository().createTransaction( "abstractprocedureimpl-getStatementOptions", true, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "getStatementOptions: transaction = {0}", transaction.getName() ); //$NON-NLS-1$
        }

        try {
            final List< StatementOption > result = new ArrayList< StatementOption >();

            for (final KomodoObject kobject : super.getChildrenOfType( transaction, StandardDdlLexicon.TYPE_STATEMENT_OPTION )) {
                final StatementOption option = new StatementOptionImpl( transaction, getRepository(), kobject.getAbsolutePath() );

                if (LOGGER.isDebugEnabled()) {
                    LOGGER.debug( "getStatementOptions: transaction = {0}, found statement option = {1}", //$NON-NLS-1$
                                  transaction.getName(),
                                  kobject.getAbsolutePath() );
                }

                result.add( option );
            }

            if (uow == null) {
                transaction.commit();
            }

            if (result.isEmpty()) {
                return StatementOption.NO_OPTIONS;
            }

            return result.toArray( new StatementOption[result.size()] );
        } catch (final Exception e) {
            throw handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.AbstractProcedure#getUpdateCount(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public int getUpdateCount( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption( transaction, this, StandardOptions.UPDATECOUNT.name() );

        if (option == null) {
            return AbstractProcedure.DEFAULT_UPDATE_COUNT;
        }

        return Integer.parseInt( option.getOption( transaction ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.AbstractProcedure#getUuid(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getUuid( final UnitOfWork transaction ) throws KException {
        final StatementOption option = Utils.getOption( transaction, this, StandardOptions.UUID.name() );

        if (option == null) {
            return null;
        }

        return option.getOption( transaction );
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
        ArgCheck.isNotEmpty( parameterName, "parameterName" ); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction( "abstractprocedureimpl-removeParameter", false, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "removeParameter: transaction = {0}, parameterName = {1}", //$NON-NLS-1$
                          transaction.getName(),
                          parameterName );
        }

        boolean found = false;

        try {
            final Parameter[] parameters = getParameters( transaction );

            if (parameters.length != 0) {
                for (final Parameter parameter : parameters) {
                    if (parameterName.equals( parameter.getName( transaction ) )) {
                        parameter.remove( transaction );
                        found = true;
                        break;
                    }
                }
            }

            if (!found) {
                throw new KException( Messages.getString( Relational.PARAMETER_NOT_FOUND_TO_REMOVE, parameterName ) );
            }

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError( uow, transaction, e );
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
        ArgCheck.isNotEmpty( optionToRemove, "optionToRemove" ); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction( "abstractprocedureimpl-removeStatementOption", false, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "removeStatementOption: transaction = {0}, optionToRemove = {1}", //$NON-NLS-1$
                          transaction.getName(),
                          optionToRemove );
        }

        boolean found = false;

        try {
            final StatementOption[] options = getStatementOptions( transaction );

            if (options.length != 0) {
                for (final StatementOption option : options) {
                    if (optionToRemove.equals( option.getName( transaction ) )) {
                        option.remove( transaction );
                        found = true;
                        break;
                    }
                }
            }

            if (!found) {
                throw new KException( Messages.getString( Relational.STATEMENT_OPTION_NOT_FOUND_TO_REMOVE, optionToRemove ) );
            }

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError( uow, transaction, e );
        }
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
        setStatementOption( transaction, StandardOptions.ANNOTATION.name(), newDescription );
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
        setStatementOption( transaction, StandardOptions.NAMEINSOURCE.name(), newNameInSource );
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
        final String newValue = ( ( newSchemaElementType == null ) ? SchemaElementType.DEFAULT_VALUE.name() : newSchemaElementType.toString() );
        setObjectProperty( uow, "setSchemaElementType", SchemaElement.TYPE, newValue ); //$NON-NLS-1$
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
        ArgCheck.isNotEmpty( optionName, "optionName" ); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction( "abstractprocedureimpl-setStatementOption", false, null ); //$NON-NLS-1$
        }

        assert ( transaction != null );

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "setStatementOption: transaction = {0}, optionName = {1}", //$NON-NLS-1$
                          transaction.getName(),
                          optionName );
        }

        try {
            StatementOption result = null;

            if (StringUtils.isBlank( optionValue )) {
                removeStatementOption( transaction, optionName );
            } else {
                result = Utils.getOption( transaction, this, optionName );

                if (result == null) {
                    result = RelationalModelFactory.createStatementOption( transaction,
                                                                           getRepository(),
                                                                           this,
                                                                           optionName,
                                                                           optionValue );
                } else {
                    result.setOption( transaction, optionValue );
                }
            }

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError( uow, transaction, e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.AbstractProcedure#setUpdateCount(org.komodo.spi.repository.Repository.UnitOfWork, int)
     */
    @Override
    public void setUpdateCount( final UnitOfWork transaction,
                                final int newUpdateCount ) throws KException {
        setStatementOption( transaction, StandardOptions.UPDATECOUNT.name(), Integer.toString( newUpdateCount ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.AbstractProcedure#setUuid(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setUuid( final UnitOfWork transaction,
                         final String newUuid ) throws KException {
        setStatementOption( transaction, StandardOptions.UUID.name(), newUuid );
    }

}
