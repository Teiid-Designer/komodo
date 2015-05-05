/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.spi.repository;

import java.math.BigDecimal;
import java.util.Calendar;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Represents a {@link KomodoObject Komodo object} property.
 */
public interface Property extends KNode {

    /**
     * An empty array of model properties.
     */
    Property[] NO_PROPS = {};

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value type of the value contained in this property
     * @throws KException
     *         if the property does not exist or an error occurs
     */
    PropertyValueType getValueType( final UnitOfWork uow ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value represented as a <code>boolean</code>
     * @throws KException
     *         if the property does not exist or an error occurs
     */
    boolean getBooleanValue( final UnitOfWork uow ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the values represented as <code>boolean</code>s (never <code>null</code> or empty)
     * @throws KException
     *         if not a multi-value property, if property does not exist, or if an error occurs
     */
    boolean[] getBooleanValues( final UnitOfWork uow ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value represented as a date or <code>null</code> if the property doesn't exist
     * @throws KException
     *         if the property does not exist or an error occurs
     */
    Calendar getDateValue( final UnitOfWork uow ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the values represented as dates (never <code>null</code> or empty)
     * @throws KException
     *         if not a multi-value property, if property does not exist, or if an error occurs
     */
    Calendar[] getDateValues( final UnitOfWork uow ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value represented as a <code>decimal</code> or <code>null</code> if the property doesn't exist
     * @throws KException
     *         if the property does not exist or an error occurs
     */
    BigDecimal getDecimalValue( final UnitOfWork uow ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the values represented as <code>decimal</code>s (never <code>null</code> or empty)
     * @throws KException
     *         if not a multi-value property, if property does not exist, or if an error occurs
     */
    BigDecimal[] getDecimalValues( final UnitOfWork uow ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the property descriptor (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    PropertyDescriptor getDescriptor( final UnitOfWork uow ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value represented as a <code>double</code> or <code>null</code> if the property doesn't exist
     * @throws KException
     *         if the property does not exist or an error occurs
     */
    double getDoubleValue( final UnitOfWork uow ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the values represented as <code>double</code>s (never <code>null</code> or empty)
     * @throws KException
     *         if not a multi-value property, if property does not exist, or if an error occurs
     */
    double[] getDoubleValues( final UnitOfWork uow ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the Long value of the supplied property, or <code>null</code> if the property doesn't exist
     * @throws KException
     *         if the property does not exist or an error occurs
     */
    long getLongValue( final UnitOfWork uow ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the long values of the supplied property (never <code>null</code> or empty)
     * @throws KException
     *         if not a multi-value property, if property does not exist, or if an error occurs
     */
    long[] getLongValues( final UnitOfWork uow ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the String value of the supplied property, or <code>null</code> if the property doesn't exist
     * @throws KException
     *         if the property does not exist or an error occurs
     */
    String getStringValue( final UnitOfWork uow ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the String values of the supplied property (never <code>null</code> or empty)
     * @throws KException
     *         if not a multi-value property, if property does not exist, or if an error occurs
     */
    String[] getStringValues( final UnitOfWork uow ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the supplied property, or <code>null</code> if the property doesn't exist
     * @throws KException
     *         if the property does not exist or an error occurs
     */
    Object getValue( final UnitOfWork uow ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the values of the supplied property (never <code>null</code> or empty)
     * @throws KException
     *         if not a multi-value property, if property does not exist, or if an error occurs
     */
    Object[] getValues( final UnitOfWork uow ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return this property has multiple values
     * @throws KException
     *         if an error occurs
     */
    boolean isMultiple( final UnitOfWork uow ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * Passing in <code>null</code> will remove the existing property from its node.
     *
     * @param values
     *        the new value for single-valued properties or the new values for multi-valued properties (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    void set(  final UnitOfWork uow,
               final Object... values ) throws KException;

}
