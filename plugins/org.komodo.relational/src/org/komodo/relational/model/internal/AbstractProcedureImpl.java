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
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.SchemaElement;

/**
 * A base implementation of a relational model procedure or function.
 */
abstract class AbstractProcedureImpl extends RelationalObjectImpl implements AbstractProcedure {

    /**
     * The allowed child types.
     */
    private static final KomodoType[] CHILD_TYPES = new KomodoType[] { Parameter.IDENTIFIER };

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
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
    public Parameter addParameter( final UnitOfWork transaction,
                                   final String parameterName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( parameterName, "parameterName" ); //$NON-NLS-1$

        final Parameter result = RelationalModelFactory.createParameter( transaction, getRepository(), this, parameterName );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoObject[] getChildren( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        KomodoObject[] result = null;
        final KomodoObject[] kids = super.getChildren( transaction );

        // do not include statement options
        if ( kids.length == 0 ) {
            result = kids;
        } else {
            final List< KomodoObject > temp = new ArrayList<>();

            for ( final KomodoObject kid : kids ) {
                if ( kid.hasDescriptor( transaction, StandardDdlLexicon.TYPE_STATEMENT_OPTION ) ) {
                    continue;
                }

                temp.add( kid );
            }

            result = temp.toArray( new KomodoObject[ temp.size() ] );
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildTypes()
     */
    @Override
    public KomodoType[] getChildTypes() {
        return CHILD_TYPES;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#getCustomOptions(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public StatementOption[] getCustomOptions( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        StatementOption[] result = getStatementOptions( transaction );

        if ( result.length != 0 ) {
            final List< StatementOption > temp = new ArrayList<>( result.length );

            for ( final StatementOption option : result ) {
                if ( StandardOptions.valueOf( option.getName( transaction ) ) == null ) {
                    temp.add( option );
                }
            }

            result = temp.toArray( new StatementOption[ temp.size() ] );
        }

        return result;
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
    public Parameter[] getParameters( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject[] parameters = super.getChildrenOfType( transaction, CreateProcedure.PARAMETER );
        Parameter[] result = null;

        if ( parameters.length == 0 ) {
            result = Parameter.NO_PARAMETERS;
        }

        result = new ParameterImpl[ parameters.length ];
        int i = 0;

        for ( final KomodoObject param : parameters ) {
            result[i] = new ParameterImpl( transaction, getRepository(), param.getAbsolutePath() );
            ++i;
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Model getParent( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject kobject = super.getParent( transaction );
        assert ( kobject instanceof Model );
        final Model result = ( Model )kobject;
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.SchemaElement#getSchemaElementType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public SchemaElementType getSchemaElementType( final UnitOfWork uow ) throws KException {
        final String value = getObjectProperty( uow, PropertyValueType.STRING, "getSchemaElementType", //$NON-NLS-1$
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
    public StatementOption[] getStatementOptions( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< StatementOption > result = new ArrayList< StatementOption >();

        for ( final KomodoObject kobject : super.getChildrenOfType( transaction, StandardDdlLexicon.TYPE_STATEMENT_OPTION ) ) {
            final StatementOption option = new StatementOptionImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            result.add( option );
        }

        if ( result.isEmpty() ) {
            return StatementOption.NO_OPTIONS;
        }

        return result.toArray( new StatementOption[ result.size() ] );
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
    public void removeParameter( final UnitOfWork transaction,
                                 final String parameterName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( parameterName, "parameterName" ); //$NON-NLS-1$

        boolean found = false;
        final Parameter[] parameters = getParameters( transaction );

        if ( parameters.length != 0 ) {
            for ( final Parameter parameter : parameters ) {
                if ( parameterName.equals( parameter.getName( transaction ) ) ) {
                    parameter.remove( transaction );
                    found = true;
                    break;
                }
            }
        }

        if ( !found ) {
            throw new KException( Messages.getString( Relational.PARAMETER_NOT_FOUND_TO_REMOVE, parameterName ) );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#removeStatementOption(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void removeStatementOption( final UnitOfWork transaction,
                                       final String optionToRemove ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( optionToRemove, "optionToRemove" ); //$NON-NLS-1$

        boolean found = false;
        final StatementOption[] options = getStatementOptions( transaction );

        if ( options.length != 0 ) {
            for ( final StatementOption option : options ) {
                if ( optionToRemove.equals( option.getName( transaction ) ) ) {
                    option.remove( transaction );
                    found = true;
                    break;
                }
            }
        }

        if ( !found ) {
            throw new KException( Messages.getString( Relational.STATEMENT_OPTION_NOT_FOUND_TO_REMOVE, optionToRemove ) );
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
    public StatementOption setStatementOption( final UnitOfWork transaction,
                                               final String optionName,
                                               final String optionValue ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( optionName, "optionName" ); //$NON-NLS-1$

        StatementOption result = null;

        if ( StringUtils.isBlank( optionValue ) ) {
            removeStatementOption( transaction, optionName );
        } else {
            result = Utils.getOption( transaction, this, optionName );

            if ( result == null ) {
                result = RelationalModelFactory.createStatementOption( transaction,
                                                                       getRepository(),
                                                                       this,
                                                                       optionName,
                                                                       optionValue );
            } else {
                result.setOption( transaction, optionValue );
            }
        }

        return result;
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
