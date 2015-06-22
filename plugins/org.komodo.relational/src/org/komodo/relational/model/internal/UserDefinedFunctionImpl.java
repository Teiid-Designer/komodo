/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.UserDefinedFunction;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.SchemaElement;

/**
 * An implementation of a UDF.
 */
public final class UserDefinedFunctionImpl extends FunctionImpl implements UserDefinedFunction {

    private enum StandardOption {

        CATEGORY,
        JAVA_CLASS,
        JAVA_METHOD;

        /**
         * @param name
         *        the name being checked (can be <code>null</code>)
         * @return <code>true</code> if the name is the name of a standard option
         */
        static boolean isValid( final String name ) {
            for ( final StandardOption option : values() ) {
                if ( option.name().equals( name ) ) {
                    return true;
                }
            }

            return false;
        }

        /**
         * @return the names of all the options (never <code>null</code> or empty)
         */
        static String[] names() {
            final StandardOption[] options = values();
            final String[] result = new String[ options.length ];
            int i = 0;

            for ( final StandardOption option : options ) {
                result[i++] = option.name();
            }

            return result;
        }

    }

    /**
     * The resolver of a {@link UserDefinedFunction}.
     */
    public static final TypeResolver< UserDefinedFunction > RESOLVER = new TypeResolver< UserDefinedFunction >() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public UserDefinedFunction create( final UnitOfWork transaction,
                                           final Repository repository,
                                           final KomodoObject parent,
                                           final String id,
                                           final RelationalProperties properties ) throws KException {
            final AdapterFactory adapter = new AdapterFactory( repository );
            final Model parentModel = adapter.adapt( transaction, parent, Model.class );

            if ( parentModel == null ) {
                throw new KException( Messages.getString( Relational.INVALID_PARENT_TYPE,
                                                          parent.getAbsolutePath(),
                                                          UserDefinedFunction.class.getSimpleName() ) );
            }

            return parentModel.addUserDefinedFunction( transaction, id );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#identifier()
         */
        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#owningClass()
         */
        @Override
        public Class< UserDefinedFunctionImpl > owningClass() {
            return UserDefinedFunctionImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final KomodoObject kobject ) throws KException {
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, CreateProcedure.FUNCTION_STATEMENT )
                   && ObjectImpl.validatePropertyValue( transaction,
                                                        kobject.getRepository(),
                                                        kobject,
                                                        SchemaElement.TYPE,
                                                        SchemaElementType.VIRTUAL.name() );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public UserDefinedFunction resolve( final UnitOfWork transaction,
                                            final KomodoObject kobject ) throws KException {
            return new UserDefinedFunctionImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

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
    public UserDefinedFunctionImpl( final UnitOfWork uow,
                                    final Repository repository,
                                    final String workspacePath ) throws KException {
        super( uow, repository, workspacePath );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.UserDefinedFunction#getCategory(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getCategory( final UnitOfWork transaction ) throws KException {
        return OptionContainerUtils.getOption( transaction, this, StandardOption.CATEGORY.name() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.UserDefinedFunction#getJavaClass(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getJavaClass( final UnitOfWork transaction ) throws KException {
        return OptionContainerUtils.getOption( transaction, this, StandardOption.JAVA_CLASS.name() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.UserDefinedFunction#getJavaMethod(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getJavaMethod( final UnitOfWork transaction ) throws KException {
        return OptionContainerUtils.getOption( transaction, this, StandardOption.JAVA_METHOD.name() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#getStandardOptionNames()
     */
    @Override
    public String[] getStandardOptionNames() {
        final String[] superNames = super.getStandardOptionNames();
        final String[] names = StandardOption.names();

        // combine
        final String[] result = new String[ superNames.length + names.length ];
        System.arraycopy( superNames, 0, result, 0, superNames.length );
        System.arraycopy( names, 0, result, superNames.length, names.length );

        return result;
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
     * @see org.komodo.repository.ObjectImpl#getTypeIdentifier(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoType getTypeIdentifier( final UnitOfWork uow ) {
        return RESOLVER.identifier();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.OptionContainer#isStandardOption(java.lang.String)
     */
    @Override
    public boolean isStandardOption( final String name ) {
        return ( super.isStandardOption( name ) || StandardOption.isValid( name ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.UserDefinedFunction#setCategory(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setCategory( final UnitOfWork transaction,
                             final String newCategory ) throws KException {
        setStatementOption( transaction, StandardOption.CATEGORY.name(), newCategory );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.UserDefinedFunction#setJavaClass(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setJavaClass( final UnitOfWork transaction,
                              final String newJavaClass ) throws KException {
        setStatementOption( transaction, StandardOption.JAVA_CLASS.name(), newJavaClass );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.UserDefinedFunction#setJavaMethod(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setJavaMethod( final UnitOfWork transaction,
                               final String newJavaMethod ) throws KException {
        setStatementOption( transaction, StandardOption.JAVA_METHOD.name(), newJavaMethod );
    }

}
