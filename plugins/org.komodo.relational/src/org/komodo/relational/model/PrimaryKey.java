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
import org.komodo.relational.model.internal.PrimaryKeyImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.Constraint;

/**
 * Represents a relational model primary key.
 */
public interface PrimaryKey extends TableConstraint {

    /**
     * The type identifier.
     */
    int TYPE_ID = PrimaryKey.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.PRIMARY_KEY;

    /**
     * The constraint type for a primary key. Value is {@value} .
     */
    ConstraintType CONSTRAINT_TYPE = ConstraintType.PRIMARY_KEY;

    /**
     * The resolver of a {@link PrimaryKey}.
     */
    public static final TypeResolver< PrimaryKey > RESOLVER = new TypeResolver< PrimaryKey >() {
    
        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public PrimaryKey create( final UnitOfWork transaction,
                                  final Repository repository,
                                  final KomodoObject parent,
                                  final String id,
                                  final RelationalProperties properties ) throws KException {
            final AdapterFactory adapter = new AdapterFactory( );
            final Table parentTable = adapter.adapt( transaction, parent, Table.class );
    
            if ( parentTable == null ) {
                throw new KException( Messages.getString( Relational.INVALID_PARENT_TYPE,
                                                          parent.getAbsolutePath(),
                                                          PrimaryKey.class.getSimpleName() ) );
            }
    
            return parentTable.setPrimaryKey( transaction, id );
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
        public Class< PrimaryKeyImpl > owningClass() {
            return PrimaryKeyImpl.class;
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
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, Constraint.TABLE_ELEMENT )
                   && ObjectImpl.validatePropertyValue( transaction,
                                                        kobject.getRepository(),
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
        public PrimaryKey resolve( final UnitOfWork transaction,
                                   final KomodoObject kobject ) throws KException {
            return new PrimaryKeyImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }
    
    };

}
