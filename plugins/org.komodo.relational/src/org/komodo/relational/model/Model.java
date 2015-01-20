/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * Represents a relational model.
 */
public interface Model extends RelationalObject {

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return model type of this model
     * @throws KException if error occurs
     */
    String getModelType(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @param modelType the model type
     * @throws KException if error occurs
     */
    void setModelType(UnitOfWork transaction, String modelType) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return model definition of this model
     * @throws KException if error occurs
     */
    String getModelDefinition(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @param modelDefinition the model definition, eg. a string of ddl
     * @throws KException  if error occurs
     */
    void setModelDefinition(UnitOfWork transaction, String modelDefinition) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @param functionName
     *        the name of the function to create (cannot be empty)
     * @return the new function (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Procedure addFunction( final UnitOfWork transaction,
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
     * No functions are returned.
     *
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the functions found in this model (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Procedure[] getFunctions( final UnitOfWork transaction ) throws KException;

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

}
