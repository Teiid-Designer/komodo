/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.model;

import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.model.internal.StatementOptionImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;

/**
 * Represents a DDL statement option from a relational model.
 */
public interface StatementOption extends Property, RelationalObject {

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.STATEMENT_OPTION;

    /**
     * An empty collection of index constraints.
     */
    StatementOption[] NO_OPTIONS = new StatementOption[0];

    /**
     * The resolver of a {@link StatementOption}.
     */
    public static final TypeResolver< StatementOption > RESOLVER = new TypeResolver< StatementOption >() {
    
        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public StatementOption create( final UnitOfWork transaction,
                                       final Repository repository,
                                       final KomodoObject parent,
                                       final String id,
                                       final RelationalProperties properties ) throws KException {
            final AdapterFactory adapter = new AdapterFactory( );
            final Object optionValueValue = properties.getValue( StandardDdlLexicon.VALUE );
            final String optionValue = optionValueValue == null ? null : optionValueValue.toString();
            final OptionContainer parentContainer = adapter.adapt( transaction, parent, OptionContainer.class );
    
            if ( parentContainer == null ) {
                throw new KException( Messages.getString( Relational.INVALID_PARENT_TYPE,
                                                          parent.getAbsolutePath(),
                                                          StatementOption.class.getSimpleName() ) );
            }
    
            return parentContainer.setStatementOption( transaction, id, optionValue );
        }
    
        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#identifier()
         */
        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }
    
        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#owningClass()
         */
        @Override
        public Class< StatementOptionImpl > owningClass() {
            return StatementOptionImpl.class;
        }
    
        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final KomodoObject kobject ) throws KException {
            return ObjectImpl.validateType( transaction,
                                            kobject.getRepository(),
                                            kobject,
                                            StandardDdlLexicon.TYPE_STATEMENT_OPTION );
        }
    
        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public StatementOption resolve( final UnitOfWork transaction,
                                        final KomodoObject kobject ) throws KException {
            return new StatementOptionImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }
    
    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the statement option (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getOption( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newOption
     *        the new value for the <code>statement option</code> property (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void setOption( final UnitOfWork transaction,
                    final String newOption ) throws KException;

}
