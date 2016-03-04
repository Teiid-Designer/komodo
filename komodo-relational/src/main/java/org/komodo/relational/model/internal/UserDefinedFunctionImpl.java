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
package org.komodo.relational.model.internal;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.komodo.relational.model.Model;
import org.komodo.relational.model.UserDefinedFunction;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;

/**
 * An implementation of a UDF.
 */
public final class UserDefinedFunctionImpl extends FunctionImpl implements UserDefinedFunction {

    private static Map< String, String > _defaultValues = null;

    private enum StandardOption {

        CATEGORY,
        JAVA_CLASS,
        JAVA_METHOD;

        /**
         * @return an unmodifiable collection of the names and default values of all the standard options (never <code>null</code>
         *         or empty)
         */
        static Map< String, String > defaultValues() {
            final StandardOption[] options = values();
            final Map< String, String > result = new HashMap< >();

            for ( final StandardOption option : options ) {
                result.put( option.name(), null ); // no default values
            }

            return Collections.unmodifiableMap( result );
        }

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
     * @see org.komodo.relational.model.OptionContainer#getStandardOptions()
     */
    @Override
    public Map< String, String > getStandardOptions() {
        if ( _defaultValues == null ) {
            final Map< String, String > superOptions = super.getStandardOptions();
            final Map< String, String > options = StandardOption.defaultValues();

            final Map< String, String > combined = new HashMap< >( superOptions.size() + options.size() );
            combined.putAll( superOptions );
            combined.putAll( options );

            _defaultValues = Collections.unmodifiableMap( combined );
        }

        return _defaultValues;
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
        return UserDefinedFunction.IDENTIFIER;
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Model getParent( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject parent = super.getParent( transaction );
        final Model result = Model.RESOLVER.resolve( transaction, parent );
        return result;
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
