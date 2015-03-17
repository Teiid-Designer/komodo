/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.spi.repository;

import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * Represents a Komodo object.
 */
public interface KomodoObject extends KNode {

    /**
     * An empty Komodo object array.
     */
    KomodoObject[] EMPTY_ARRAY = new KomodoObject[0];

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @return Type identifier of this komodo object
     * @throws KException if error occurs
     */
    KomodoType getTypeIdentifier(UnitOfWork transaction) throws KException;

    /**
     * Adds a child with the supplied name and primary type.
     *
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param name
     *        the name of the new child being added (cannot be empty)
     * @param primaryType
     *        the primary type of the child or <code>null</code> if type is <code>nt:unstructured</code>
     * @param saveSession
     *        <code>true</code> if the session should be saved after creating the Komodo object
     * @return the new object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    KomodoObject addChild( final UnitOfWork transaction,
                           final String name,
                           final String primaryType ) throws KException;

    /**
     * Adds the specified descriptor name(s).
     *
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param descriptorNames
     *        the descriptor name(s) being added (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void addDescriptor( final UnitOfWork transaction,
                        final String... descriptorNames ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @param name
     *        the name of child being requested (cannot be empty)
     * @return the child object (never <code>null</code>)
     * @throws KException
     *         if the child does not exist or an error occurs
     */
    KomodoObject getChild( final UnitOfWork transaction,
                           final String name ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the child objects (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    KomodoObject[] getChildren( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @param name
     *        the name of child(ren) being requested (cannot be empty)
     * @return the child object(s) (never <code>null</code> but can be empty)
     * @throws KException
     *         if the child does not exist or an error occurs
     */
    KomodoObject[] getChildren( final UnitOfWork transaction,
                                final String name ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @param type
     *        the primary type or mixin of the children being requested (cannot be empty)
     * @return the matching children (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    KomodoObject[] getChildrenOfType( final UnitOfWork transaction,
                                      final String type ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the object's mixin type descriptors (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Descriptor[] getDescriptors( final UnitOfWork transaction ) throws KException;

    /**
     * @return the object's zero-based index relative to any other same-name-siblings or -1 if there is no same-name-siblings
     */
    int getIndex();

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return this object's primary type descriptor (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Descriptor getPrimaryType( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @param name
     *        the name of property being requested (cannot be empty)
     * @return the property or <code>null</code> if the property doesn't exist
     * @throws KException
     *         if an error occurs
     */
    Property getProperty( final UnitOfWork transaction,
                          final String name ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the property names for this object
     * @throws KException
     *         if an error occurs
     */
    String[] getPropertyNames( final UnitOfWork transaction ) throws KException;

    /**
     * @return a unique identifier for the object class
     */
    int getTypeId();

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @param name
     *        the name of the child whose existence is being checked (cannot be empty)
     * @return <code>true</code> if a child with the supplied name exists
     * @throws KException
     *         if an error occurs
     */
    boolean hasChild( final UnitOfWork transaction,
                      final String name ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if children exist
     * @throws KException
     *         if an error occurs
     */
    boolean hasChildren( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @param descriptorName
     *        the name of the {@link Descriptor descriptor} being checked (cannot be empty)
     * @return <code>true</code> if this {@link KomodoObject} has this descriptor
     * @throws KException
     *         if an error occurs
     */
    boolean hasDescriptor( final UnitOfWork transaction,
                           final String descriptorName ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if properties exist
     * @throws KException
     *         if an error occurs
     */
    boolean hasProperties( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @param name
     *        the name the property whose existence is being checked (cannot be empty)
     * @return <code>true</code> if a property with the supplied name exists
     * @throws KException
     *         if an error occurs
     */
    boolean hasProperty( final UnitOfWork transaction,
                         final String name ) throws KException;

    /**
     * Prints this object's subtree to standard out
     *
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @throws KException
     *         if an error occurs
     */
    void print( final UnitOfWork transaction ) throws KException;

    /**
     * Removes this node.
     *
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @throws KException
     *         if an error occurs
     */
    void remove( final UnitOfWork transaction ) throws KException;

    /**
     * To remove children with same name, the same name must be passed in more than once. It is recommended to use
     * {@link #remove(UnitOfWork)} whenever the node being removed is available.
     *
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param names
     *        the name(s) of the child(ren) being removed from this object (cannot be empty)
     * @throws KException
     *         if an error occurs
     * @see #remove(UnitOfWork)
     */
    void removeChild( final UnitOfWork transaction,
                      final String... names ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param descriptorNames
     *        the mixin(s) being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeDescriptor( final UnitOfWork transaction,
                           final String... descriptorNames ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newName
     *        the new name (cannot be empty or contain any slashes)
     * @throws KException
     *         if an error occurs
     */
    void rename( final UnitOfWork transaction,
                 final String newName ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param typeName
     *        the new primary type name or <code>null</code> or empty if setting to <code>nt:unstructured</code>
     * @throws KException
     *         if an error occurs
     */
    void setPrimaryType( final UnitOfWork transaction,
                         final String typeName ) throws KException;

    /**
     * Creates the property if it does not exist. Passing a <code>null</code> value will remove the property. Passing multiple
     * values should only be used for creating and setting multi-valued properties.
     *
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param propertyName
     *        the name of one of this model object's properties (cannot be <code>null</code> or empty)
     * @param values
     *        one or more new values (can be <code>null</code> or empty)
     * @throws KException
     *         if an error occurs
     * @throws IllegalArgumentException
     *         if trying to set a single-valued property with multiple values or trying to set values that are not compatible with
     *         the property definition
     */
    void setProperty( final UnitOfWork transaction,
                      final String propertyName,
                      final Object... values ) throws KException;

    /**
     * Visit this object with the given visitor
     *
     * @param visitor the visitor
     * @throws Exception if error occurs
     */
    void visit(KomodoObjectVisitor visitor) throws Exception;
}
