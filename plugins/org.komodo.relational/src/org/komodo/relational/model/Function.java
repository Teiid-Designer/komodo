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
 * Represents a user-defined scalar function.
 */
public interface Function extends AbstractProcedure {

    /**
     * The categories of a function's determinism.
     */
    public enum Determinism {

        /**
         * The result of function evaluation is only deterministic within the scope of the user command.
         */
        COMMAND_DETERMINISTIC,

        /**
         * The function will always return the same result for the given inputs.
         */
        DETERMINISTIC,

        /**
         * The result of function evaluation is fully nondeterministic.
         */
        NONDETERMINISTIC,

        /**
         * The function will return the same result for the given inputs under the same user session.
         */
        SESSION_DETERMINISTIC,

        /**
         * The function will return the same result for the given inputs for the same user.
         */
        USER_DETERMINISTIC,

        /**
         * The function will always return the same result for the given inputs.
         */
        VDB_DETERMINISTIC;

        /**
         * The default value is {@value} .
         */
        public static final Determinism DEFAULT_VALUE = DETERMINISTIC;

    }

    /**
     * The default value for the <code>aggregate</code> property. Value is {@value} .
     */
    boolean DEFAULT_AGGREGATE = false;

    /**
     * The default value for the <code>allows distinct</code> property. Value is {@value} .
     */
    boolean DEFAULT_ALLOWS_DISTINCT = false;

    /**
     * The default value for the <code>allows order by</code> property. Value is {@value} .
     */
    boolean DEFAULT_ALLOWS_ORDER_BY = false;

    /**
     * The default value for the <code>analytic</code> property. Value is {@value} .
     */
    boolean DEFAULT_ANALYTIC = false;

    /**
     * The default value for the <code>decomposable</code> property. Value is {@value} .
     */
    boolean DEFAULT_DECOMPOSABLE = false;

    /**
     * The default value for the <code>null on null</code> property. Value is {@value} .
     */
    boolean DEFAULT_NULL_ON_NULL = false;

    /**
     * The default value for the <code>uses distinct rows</code> property. Value is {@value} .
     */
    boolean DEFAULT_USES_DISTINCT_ROWS = false;

    /**
     * The default value for the <code>varargs</code> property. Value is {@value} .
     */
    boolean DEFAULT_VARARGS = false;

    /**
     * An empty array of functions.
     */
    Function[] NO_FUNCTIONS = new Function[0];

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
     * @return the value of the <code>determinism</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    Determinism getDeterminism( final UnitOfWork transaction ) throws KException;

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
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if the function is an aggregate
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_AGGREGATE
     */
    boolean isAggregate( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if the function allows the DISTINCT keyword
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOWS_DISTINCT
     */
    boolean isAllowsDistinct( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if the function supports the ORDER BY clause
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOWS_ORDER_BY
     */
    boolean isAllowsOrderBy( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if the function is analytic
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ANALYTIC
     */
    boolean isAnalytic( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if the function is decomposable
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_DECOMPOSABLE
     */
    boolean isDecomposable( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if the function is called when any of its parameters is <code>null</code>
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_NULL_ON_NULL
     */
    boolean isNullOnNull( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if the function uses distinct rows
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_USES_DISTINCT_ROWS
     */
    boolean isUsesDistinctRows( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if the function has variable arguments
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_VARARGS
     */
    boolean isVarArgs( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newAggregate
     *        the new value for the <code>aggregate</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_AGGREGATE
     */
    void setAggregate( final UnitOfWork transaction,
                       final boolean newAggregate ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newAllowsDistinct
     *        the new value for the <code>allows distinct</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOWS_DISTINCT
     */
    void setAllowsDistinct( final UnitOfWork transaction,
                            final boolean newAllowsDistinct ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newAllowsOrderBy
     *        the new value for the <code>allows order by</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOWS_ORDER_BY
     */
    void setAllowsOrderBy( final UnitOfWork transaction,
                           final boolean newAllowsOrderBy ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newIsAnalytic
     *        the new value for the <code>analytic</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ANALYTIC
     */
    void setAnalytic( final UnitOfWork transaction,
                      final boolean newIsAnalytic ) throws KException;

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
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newDecomposable
     *        the new value for the <code>decomposable</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_DECOMPOSABLE
     */
    void setDecomposable( final UnitOfWork transaction,
                          final boolean newDecomposable ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newDeterminism
     *        the new value for the <code>determinism</code> property
     * @throws KException
     *         if an error occurs
     */
    void setDeterminism( final UnitOfWork transaction,
                         final Determinism newDeterminism ) throws KException;

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

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newNullOnNull
     *        the new value for the <code>null on null</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_NULL_ON_NULL
     */
    void setNullOnNull( final UnitOfWork transaction,
                        final boolean newNullOnNull ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newUsesDistinctRows
     *        the new value for the <code>uses distinct rows</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_USES_DISTINCT_ROWS
     */
    void setUsesDistinctRows( final UnitOfWork transaction,
                              final boolean newUsesDistinctRows ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newVarArgs
     *        the new value for the <code>variable arguments</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_VARARGS
     */
    void setVarArgs( final UnitOfWork transaction,
                     final boolean newVarArgs ) throws KException;

}
