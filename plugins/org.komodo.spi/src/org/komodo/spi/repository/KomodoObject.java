/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.spi.repository;

import org.komodo.spi.KException;
import org.komodo.spi.repository.IRepository.UnitOfWork;

/**
 * Represents a Komodo object.
 */
public interface KomodoObject extends KNode {

    /**
     * An empty Komodo object array.
     */
    KomodoObject[] EMPTY_ARRAY = new KomodoObject[0];

    /**
     * Adds a child with the supplied name and primary type. Creates then commits a transaction containing only this operation.
     *
     * @param name
     *        the name of the new child being added (cannot be empty)
     * @param primaryType
     *        the primary type of the child or <code>null</code> if type is <code>nt:unstructured</code>
     * @return the new object (never <code>null</code>)
     * @throws KException
     *         if any error occurs
     */
    KomodoObject addChild( final String name,
                           final String primaryType ) throws KException;

    /**
     * Adds a child with the supplied name and primary type. The transaction must be committed by the caller.
     *
     * @param transaction
     *        the transaction this operation is being added to (cannot be <code>null</code>)
     * @param name
     *        the name of the new child being added (cannot be empty)
     * @param primaryType
     *        the primary type of the child or <code>null</code> if type is <code>nt:unstructured</code>
     * @param saveSession
     *        <code>true</code> if the session should be saved after creating the Komodo object
     * @return the new object (never <code>null</code>)
     * @throws KException
     *         if any error occurs
     */
    KomodoObject addChild( final org.komodo.spi.repository.IRepository.UnitOfWork transaction,
                           final String name,
                           final String primaryType ) throws KException;

    /**
     * Adds the specified mixin(s). Creates then commits a transaction containing only this operation.
     *
     * @param mixins
     *        the mixin(s) being added (cannot be empty)
     * @throws KException
     *         if any error occurs
     */
    void addMixin( final String... mixins ) throws KException;

    /**
     * Adds the specified mixin(s). The transaction must be committed by the caller.
     *
     * @param transaction
     *        the transaction this operation is being added to (cannot be <code>null</code>)
     * @param mixins
     *        the mixin(s) being added (cannot be empty)
     * @throws KException
     *         if any error occurs
     */
    void addMixin( final UnitOfWork transaction,
                   final String... mixins ) throws KException;

    /**
     * @param name
     *        the name of child being requested (cannot be empty)
     * @return the child object (never <code>null</code>)
     * @throws KException
     *         if the child does not exist or any error occurs
     */
    KomodoObject getChild( final String name ) throws KException;

    /**
     * @return the child objects (never <code>null</code> but can be empty)
     * @throws KException
     *         if any error occurs
     */
    KomodoObject[] getChildren() throws KException;

    /**
     * @param name
     *        the name of child(ren) being requested (cannot be empty)
     * @return the child object(s) (never <code>null</code> or empty)
     * @throws KException
     *         if the child does not exist or any error occurs
     */
    KomodoObject[] getChildren( final String name ) throws KException;

    /**
     * @param primaryType
     *        the primary type of the children being requested (cannot be empty)
     * @return the matching children (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    KomodoObject[] getChildrenOfType( final String primaryType ) throws KException;

    /**
     * @return the object's zero-based index relative to any other same-name-siblings or -1 if there is no same-name-siblings
     */
    int getIndex();

    /**
     * @return the object's mixin type descriptors (never <code>null</code> but can be empty)
     * @throws KException
     *         if any error occurs
     */
    Descriptor[] getMixins() throws KException;

    /**
     * @return this object's primary type descriptor (never <code>null</code>)
     * @throws KException
     *         if any error occurs
     */
    Descriptor getPrimaryType() throws KException;

    /**
     * @param name
     *        the name of property being requested (cannot be empty)
     * @return the property or <code>null</code> if the property doesn't exist
     * @throws KException
     *         if any error occurs
     */
    Property getProperty( final String name ) throws KException;

    /**
     * @return the property names for this object
     * @throws KException
     *         if any error occurs
     */
    String[] getPropertyNames() throws KException;

    /**
     * @param name
     *        the name of the child whose existence is being checked (cannot be empty)
     * @return <code>true</code> if a child with the supplied name exists
     * @throws KException
     *         if any error occurs
     */
    boolean hasChild( final String name ) throws KException;

    /**
     * @return <code>true</code> if children exist
     * @throws KException
     *         if any error occurs
     */
    boolean hasChildren() throws KException;

    /**
     * @return <code>true</code> if properties exist
     * @throws KException
     *         if any error occurs
     */
    boolean hasProperties() throws KException;

    /**
     * @param name
     *        the name the property whose existence is being checked (cannot be empty)
     * @return <code>true</code> if a property with the supplied name exists
     * @throws KException
     *         if any error occurs
     */
    boolean hasProperty( final String name ) throws KException;

    /**
     * Prints this object's subtree to standard out
     *
     * @throws KException
     *         if any error occurs
     */
    void print() throws KException;

    /**
     * To remove children with same name, the same name must be passed in more than once. Creates then commits a transaction
     * containing only this operation.
     *
     * @param names
     *        the name(s) of the child(ren) being removed from this object (cannot be empty)
     * @throws KException
     *         if any error occurs
     */
    void removeChild( final String... names ) throws KException;

    /**
     * To remove children with same name, the same name must be passed in more than once. The transaction must be committed by the
     * caller.
     *
     * @param transaction
     *        the transaction this operation is being added to (cannot be <code>null</code>)
     * @param names
     *        the name(s) of the child(ren) being removed from this object (cannot be empty)
     * @throws KException
     *         if any error occurs
     */
    void removeChild( final UnitOfWork transaction,
                      final String... names ) throws KException;

    /**
     * Creates then commits a transaction containing only this operation.
     *
     * @param saveSession
     *        <code>true</code> if the session should be saved after creating the Komodo object
     * @param mixins
     *        the mixin(s) being removed (cannot be empty)
     * @throws KException
     *         if any error occurs
     */
    void removeMixin( final String... mixins ) throws KException;

    /**
     * The transaction must be committed by the caller.
     *
     * @param transaction
     *        the transaction this operation is being added to (cannot be <code>null</code>)
     * @param mixins
     *        the mixin(s) being removed (cannot be empty)
     * @throws KException
     *         if any error occurs
     */
    void removeMixin( final UnitOfWork transaction,
                      final String... mixins ) throws KException;

    /**
     * Creates then commits a transaction containing only this operation.
     *
     * @param typeName
     *        the new primary type name or <code>null</code> if setting to <code>nt:unstructured</code>
     * @param saveSession
     *        <code>true</code> if the session should be saved after creating the Komodo object
     * @throws KException
     *         if any error occurs
     */
    void setPrimaryType( final String typeName ) throws KException;

    /**
     * The transaction must be committed by the caller.
     *
     * @param transaction
     *        the transaction this operation is being added to (cannot be <code>null</code>)
     * @param typeName
     *        the new primary type name or <code>null</code> if setting to <code>nt:unstructured</code>
     * @throws KException
     *         if any error occurs
     */
    void setPrimaryType( final UnitOfWork transaction,
                         final String typeName ) throws KException;

    /**
     * Creates the property if it does not exist. Passing a <code>null</code> value will remove the property. Passing multiple
     * values should only be used for creating and setting multi-valued properties. Creates then commits a transaction containing
     * only this operation.
     *
     * @param saveSession
     *        <code>true</code> if the session should be saved after creating the Komodo object
     * @param propertyName
     *        the name of one of this model object's properties (cannot be <code>null</code> or empty)
     * @param values
     *        one or more new values
     * @throws KException
     *         if an error occurs
     * @throws IllegalArgumentException
     *         if trying to set a single-valued property with multiple values or trying to set values that are not compatible with
     *         the property definition
     */
    void setProperty( final String propertyName,
                      final Object... values ) throws KException;

    /**
     * Creates the property if it does not exist. Passing a <code>null</code> value will remove the property. Passing multiple
     * values should only be used for creating and setting multi-valued properties. The transaction must be committed by the
     * caller.
     *
     * @param transaction
     *        the transaction this operation is being added to (cannot be <code>null</code>)
     * @param propertyName
     *        the name of one of this model object's properties (cannot be <code>null</code> or empty)
     * @param values
     *        one or more new values
     * @throws KException
     *         if an error occurs
     * @throws IllegalArgumentException
     *         if trying to set a single-valued property with multiple values or trying to set values that are not compatible with
     *         the property definition
     */
    void setProperty( final UnitOfWork transaction,
                      final String propertyName,
                      final Object... values ) throws KException;

}
