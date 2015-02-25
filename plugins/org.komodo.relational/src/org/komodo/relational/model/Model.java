/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import org.komodo.relational.vdb.ModelSource;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * Represents a relational model.
 */
public interface Model extends RelationalObject {

    /**
     * The type identifier.
     */
    int TYPE_ID = Model.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.MODEL;

    /**
     * The type of a model.
     */
    enum Type {

        PHYSICAL,
        VIRTUAL;

        /**
         * The default model type. Value is {@value} .
         */
        public static final Type DEFAULT = PHYSICAL;

    }

    /**
     * An empty array of models.
     */
    Model[] NO_MODELS = new Model[0];

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @param functionName
     *        the name of the function to create (cannot be empty)
     * @return the new function (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Function addFunction( final UnitOfWork transaction,
                          final String functionName ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @param procedureName
     *        the name of the procedure to create (cannot be empty)
     * @return the new procedure (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Procedure addProcedure( final UnitOfWork transaction,
                            final String procedureName ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
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
     *        the transaction (can be <code>null</code> if query should be automatically committed)
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
     *        the transaction (can be <code>null</code> if query should be automatically committed)
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
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>description</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getDescription( final UnitOfWork transaction ) throws KException;

    /**
     * No functions are returned.
     *
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the functions found in this model (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Function[] getFunctions( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return model definition of this model
     * @throws KException
     *         if error occurs
     */
    String getModelDefinition( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return model type of this model (never <code>null</code>)
     * @throws KException
     *         if error occurs
     * @see Type#DEFAULT
     */
    Type getModelType( final UnitOfWork transaction ) throws KException;

    /**
     * No functions are returned.
     *
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the procedures found in this model (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Procedure[] getProcedures( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the model sources found in this model (can be empty)
     * @throws KException
     *         if an error occurs
     */
    ModelSource[] getSources( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the tables found in this model (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Table[] getTables( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the views found in this model (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    View[] getViews( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @param functionName
     *        the name of the function being deleted (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeFunction( final UnitOfWork transaction,
                         final String functionName ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @param procedureName
     *        the name of the procedure being deleted (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeProcedure( final UnitOfWork transaction,
                          final String procedureName ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @param sourceName
     *        the name of the model source being deleted (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeSource( final UnitOfWork transaction,
                       final String sourceName ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @param tableName
     *        the name of the table being deleted (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeTable( final UnitOfWork transaction,
                      final String tableName ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @param viewName
     *        the name of the view being deleted (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeView( final UnitOfWork transaction,
                     final String viewName ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newDescription
     *        the new value of the <code>description</code> property (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setDescription( final UnitOfWork transaction,
                         final String newDescription ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @param modelDefinition
     *        the model definition, eg. a string of ddl
     * @throws KException
     *         if error occurs
     */
    void setModelDefinition( UnitOfWork transaction,
                             String modelDefinition ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @param newModelType
     *        the new model type (can be <code>null</code>)
     * @throws KException
     *         if error occurs
     * @see Type#DEFAULT
     */
    void setModelType( final UnitOfWork transaction,
                       final Type newModelType ) throws KException;

}
