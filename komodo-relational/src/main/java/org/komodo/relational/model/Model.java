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
package org.komodo.relational.model;

import org.komodo.relational.RelationalObject;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.model.internal.ModelImpl;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Vdb;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * Represents a relational model.
 */
public interface Model extends Exportable, RelationalObject {

    /**
     * The default value for the <code>metadataType</code> property. Value is {@value} .
     */
    String DEFAULT_METADATA_TYPE = "DDL"; //$NON-NLS-1$

    /**
     * The default value for the <code>is visible</code> property. Value is {@value} .
     */
    boolean DEFAULT_VISIBLE = true;

    /**
     * The type identifier.
     */
    int TYPE_ID = Model.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.MODEL;

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    Vdb getParent( final UnitOfWork transaction ) throws KException;

    /**
     * The type of a model.
     */
    enum Type {

        PHYSICAL,
        VIRTUAL;

        /**
         * The default model type. Value is {@value} .
         */
        public static final Type DEFAULT_VALUE = PHYSICAL;

        public static Type findType(String typeId) {
            for (Type type : Type.values()) {
                if (type.name().equalsIgnoreCase(typeId))
                    return type;
            }

            return null;
        }
    }

    /**
     * An empty array of models.
     */
    Model[] NO_MODELS = new Model[0];

    /**
     * The resolver of a {@link Model}.
     */
    TypeResolver< Model > RESOLVER = new TypeResolver< Model >() {

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
        public Class< ModelImpl > owningClass() {
            return ModelImpl.class;
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
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, VdbLexicon.Vdb.DECLARATIVE_MODEL );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Model resolve( final UnitOfWork transaction,
                              final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == Model.TYPE_ID ) {
                return ( Model )kobject;
            }

            return new ModelImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param functionName
     *        the name of the function to create (cannot be empty)
     * @return the new function (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    PushdownFunction addPushdownFunction( final UnitOfWork transaction,
                                          final String functionName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param procedureName
     *        the name of the procedure to create (cannot be empty)
     * @return the new procedure (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    StoredProcedure addStoredProcedure( final UnitOfWork transaction,
                                        final String procedureName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param functionName
     *        the name of the function to create (cannot be empty)
     * @return the new function (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    UserDefinedFunction addUserDefinedFunction( final UnitOfWork transaction,
                                                final String functionName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param procedureName
     *        the name of the procedure to create (cannot be empty)
     * @return the new procedure (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    VirtualProcedure addVirtualProcedure( final UnitOfWork transaction,
                                          final String procedureName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param sourceName
     *        the name of the model source to create (cannot be empty)
     * @return the new model source (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    ModelSource addSource( final UnitOfWork transaction,
                           final String sourceName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param tableName
     *        the name of the table to create (cannot be empty)
     * @return the new table (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Table addTable( final UnitOfWork transaction,
                    final String tableName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param viewName
     *        the name of the view to create (cannot be empty)
     * @return the new view (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    View addView( final UnitOfWork transaction,
                  final String viewName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>description</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getDescription( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the functions found in this model (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Function[] getFunctions( final UnitOfWork transaction,
                             final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and its state must be {@link State#NOT_STARTED})
     * @return the metadata type (can be empty if there is no model definition)
     * @throws KException
     *         if error occurs
     * @see #DEFAULT_METADATA_TYPE
     */
    String getMetadataType( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the model definition of this model (can be empty)
     * @throws KException
     *         if error occurs
     */
    String getModelDefinition( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return model type of this model (never <code>null</code>)
     * @throws KException
     *         if error occurs
     * @see Type#DEFAULT_VALUE
     */
    Type getModelType( final UnitOfWork transaction ) throws KException;

    /**
     * No functions are returned.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the procedures found in this model (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Procedure[] getProcedures( final UnitOfWork transaction,
                               final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the model sources found in this model (can be empty)
     * @throws KException
     *         if an error occurs
     */
    ModelSource[] getSources( final UnitOfWork transaction,
                              final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the tables found in this model (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Table[] getTables( final UnitOfWork transaction,
                       final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the views found in this model (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    View[] getViews( final UnitOfWork transaction,
                     final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and its state must be {@link State#NOT_STARTED})
     * @return <code>true</code> if this model is visible
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_VISIBLE
     */
    boolean isVisible( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param functionName
     *        the name of the function being deleted (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeFunction( final UnitOfWork transaction,
                         final String functionName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param procedureName
     *        the name of the procedure being deleted (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeProcedure( final UnitOfWork transaction,
                          final String procedureName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param sourceName
     *        the name of the model source being deleted (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeSource( final UnitOfWork transaction,
                       final String sourceName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param tableName
     *        the name of the table being deleted (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeTable( final UnitOfWork transaction,
                      final String tableName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param viewName
     *        the name of the view being deleted (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeView( final UnitOfWork transaction,
                     final String viewName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newDescription
     *        the new value of the <code>description</code> property (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setDescription( final UnitOfWork transaction,
                         final String newDescription ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param newMetadataType
     *        the new value of the <code>metadataType</code> property (can be empty)
     * @throws KException
     *         if error occurs
     */
    void setMetadataType( final UnitOfWork transaction,
                          final String newMetadataType ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param modelDefinition
     *        the model definition, eg. a string of ddl
     * @throws KException
     *         if error occurs
     */
    void setModelDefinition( final UnitOfWork transaction,
                             final String modelDefinition ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newModelType
     *        the new model type (can be <code>null</code>)
     * @throws KException
     *         if error occurs
     * @see Type#DEFAULT_VALUE
     */
    void setModelType( final UnitOfWork transaction,
                       final Type newModelType ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param newVisible
     *        the new value for the <code>visible</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_VISIBLE
     */
    void setVisible( final UnitOfWork transaction,
                     final boolean newVisible ) throws KException;

}
