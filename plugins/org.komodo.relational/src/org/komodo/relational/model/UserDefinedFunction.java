/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * Represents a user-defined function (CREATE VIRTUAL FUNCTION).
 */
public interface UserDefinedFunction extends Function {

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.USER_DEFINED_FUNCTION;

    /**
     * An empty array of UDFs.
     */
    UserDefinedFunction[] NO_UDFS = new UserDefinedFunction[0];

    /**
     * The type identifier.
     */
    int TYPE_ID = UserDefinedFunction.class.hashCode();

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if the query should be automatically committed)
     * @return the value of the <code>category</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getCategory( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if the query should be automatically committed)
     * @return the value of the <code>Java class name</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getJavaClass( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if the query should be automatically committed)
     * @return the value of the <code>Java method name</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getJavaMethod( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if the update should be automatically committed)
     * @param newCategory
     *        the new value of the <code>category</code> property (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setCategory( final UnitOfWork transaction,
                      final String newCategory ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if the update should be automatically committed)
     * @param newJavaClass
     *        the new value of the <code>Java class name</code> property (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setJavaClass( final UnitOfWork transaction,
                       final String newJavaClass ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if the update should be automatically committed)
     * @param newJavaMethod
     *        the new value of the <code>Java method name</code> property (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setJavaMethod( final UnitOfWork transaction,
                        final String newJavaMethod ) throws KException;

}
