/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import org.komodo.relational.Messages;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.model.internal.AccessPatternImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.Constraint;


/**
 * Represents a relational model access pattern.
 */
public interface AccessPattern extends TableConstraint {

    /**
     * The type identifier.
     */
    int TYPE_ID = AccessPattern.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.ACCESS_PATTERN;

    /**
     * The constraint type for an access pattern. Value is {@value} .
     */
    ConstraintType CONSTRAINT_TYPE = ConstraintType.ACCESS_PATTERN;

    /**
     * An empty collection of access pattern constraints.
     */
    AccessPattern[] NO_ACCESS_PATTERNS = new AccessPattern[0];

    /**
     * The resolver of a {@link AccessPattern}.
     */
    public static final TypeResolver< AccessPattern > RESOLVER = new TypeResolver< AccessPattern >() {
    
        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public AccessPattern create( final UnitOfWork transaction,
                                     final Repository repository,
                                     final KomodoObject parent,
                                     final String id,
                                     final RelationalProperties properties ) throws KException {
            final AdapterFactory adapter = new AdapterFactory( );
            final Table parentTable = adapter.adapt( transaction, parent, Table.class );
    
            if ( parentTable == null ) {
                throw new KException( Messages.getString( Relational.INVALID_PARENT_TYPE,
                                                          parent.getAbsolutePath(),
                                                          AccessPattern.class.getSimpleName() ) );
            }
    
            return parentTable.addAccessPattern( transaction, id );
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
        public Class< AccessPatternImpl > owningClass() {
            return AccessPatternImpl.class;
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
            final Repository repository = kobject.getRepository();
    
            return ObjectImpl.validateType( transaction, repository, kobject, Constraint.TABLE_ELEMENT )
                   && ObjectImpl.validatePropertyValue( transaction,
                                                        repository,
                                                        kobject,
                                                        Constraint.TYPE,
                                                        CONSTRAINT_TYPE.toValue() );
        }
    
        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public AccessPattern resolve( final UnitOfWork transaction,
                                      final KomodoObject kobject ) throws KException {
            return new AccessPatternImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }
    };

}
