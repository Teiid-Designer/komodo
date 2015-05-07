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
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Represents a Komodo object.
 */
public interface KomodoObject extends KNode {

    /**
     * An empty Komodo object array.
     */
    KomodoObject[] EMPTY_ARRAY = new KomodoObject[ 0 ];

    /**
     * Adds a child with the supplied name and primary type.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @param descriptorNames
     *        the descriptor name(s) being added (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void addDescriptor( final UnitOfWork transaction,
                        final String... descriptorNames ) throws KException;

    /**
     * Obtains the first child with the specified name regardless of the type.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @param name
     *        the name of child being requested (cannot be empty)
     * @return the child object (never <code>null</code>)
     * @throws KException
     *         if the child does not exist or an error occurs
     */
    KomodoObject getChild( final UnitOfWork transaction,
                           final String name ) throws KException;

    /**
     * Obtains the first child with the specified name having the specified primary type or mixin.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @param name
     *        the name of child being requested (cannot be empty)
     * @param typeName
     *        the primary type or mixin (cannot be empty)
     * @return the child object (never <code>null</code>)
     * @throws KException
     *         if the child does not exist or an error occurs
     * @see #getChild(UnitOfWork, String)
     */
    KomodoObject getChild( final UnitOfWork transaction,
                           final String name,
                           final String typeName ) throws KException;

    /**
     * Subclasses may choose to implement this so that it may not represent that actual, physical children.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @return the child objects (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     * @see #getRawChildren(UnitOfWork)
     */
    KomodoObject[] getChildren( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
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
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @param type
     *        the primary type or mixin of the children being requested (cannot be empty)
     * @return the matching children (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    KomodoObject[] getChildrenOfType( final UnitOfWork transaction,
                                      final String type ) throws KException;

    /**
     * @return the types of children that can be created (never <code>null</code> but can be empty if any type is allowed)
     * @see #isChildRestricted()
     */
    KomodoType[] getChildTypes();

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @param typeName
     *        the name of the primary type or mixin whose descriptor is being requested (cannot be empty)
     * @return the type descriptor (never <code>null</code>)
     * @throws KException
     *         if descriptor is not found or an error occurs
     */
    Descriptor getDescriptor( final UnitOfWork transaction,
                              final String typeName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
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
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @return this object's primary type descriptor (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Descriptor getPrimaryType( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
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
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @param propName
     *        the name of the property whose descriptor is being requested (cannot be empty)
     * @return the property descriptor (can be <code>null</code> if not found)
     * @throws KException
     *         if an error occurs
     */
    PropertyDescriptor getPropertyDescriptor( final UnitOfWork transaction,
                                              final String propName ) throws KException;

    /**
     * Subclasses may choose to implement this so that it may not represent that actual, physical property names.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @return the property names for this object (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     * @see #getRawPropertyNames(UnitOfWork)
     */
    String[] getPropertyNames( final UnitOfWork transaction ) throws KException;

    /**
     * Subclasses may implement {@link #getChildren(UnitOfWork)} in such a way that it does not represent the actual set of child
     * nodes. This method obtains the actual child nodes.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @return the unfiltered child objects (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    KomodoObject[] getRawChildren( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @return the unfiltered object's mixin type descriptors (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Descriptor[] getRawDescriptors( final UnitOfWork transaction ) throws KException;

    /**
     * Obtains a property even if it has been filtered out by the subclasses.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @param name
     *        the name of property, filtered or unfiltered, being requested (cannot be empty)
     * @return the property or <code>null</code> if the property doesn't exist
     * @throws KException
     *         if an error occurs
     */
    Property getRawProperty( final UnitOfWork transaction,
                             final String name ) throws KException;

    /**
     * Subclasses may implement {@link #getPropertyNames(UnitOfWork)} in such a way that it does not represent the actual set of
     * properties. This method obtains the actual, physical set of property names.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @return the unfiltered property names for this object (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     * @see #getPropertyNames(UnitOfWork)
     */
    String[] getRawPropertyNames( final UnitOfWork transaction ) throws KException;

    /**
     * @return a unique identifier for the object class
     */
    int getTypeId();

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @return the type identifier of this komodo object (never <code>null</code>)
     * @throws KException
     *         if error occurs
     */
    KomodoType getTypeIdentifier( final UnitOfWork transaction ) throws KException;

    /**
     * Indicates if a child exists with the specified name, regardless of the type.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @param name
     *        the name of the child whose existence is being checked (cannot be empty)
     * @return <code>true</code> if a child with the supplied name exists
     * @throws KException
     *         if an error occurs
     */
    boolean hasChild( final UnitOfWork transaction,
                      final String name ) throws KException;

    /**
     * Indicates if a child exists with the specified name and the specified primary type or mixin.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @param name
     *        the name of the child whose existence is being checked (cannot be empty)
     * @param typeName
     *        the primary type or mixin (cannot be empty)
     * @return <code>true</code> if a child with the supplied name and type exists
     * @throws KException
     *         if an error occurs
     */
    boolean hasChild( final UnitOfWork transaction,
                      final String name,
                      final String typeName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @return <code>true</code> if children exist
     * @throws KException
     *         if an error occurs
     */
    boolean hasChildren( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
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
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @return <code>true</code> if properties exist
     * @throws KException
     *         if an error occurs
     */
    boolean hasProperties( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @param name
     *        the name the property whose existence is being checked (cannot be empty)
     * @return <code>true</code> if a property with the supplied name exists
     * @throws KException
     *         if an error occurs
     */
    boolean hasProperty( final UnitOfWork transaction,
                         final String name ) throws KException;

    /**
     * The implementing class is responsible for enforcing this restriction.
     *
     * @return <code>true</code> if children are not permitted
     */
    boolean isChildRestricted();

    /**
     * Prints this object's subtree to standard out
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @throws KException
     *         if an error occurs
     */
    void print( final UnitOfWork transaction ) throws KException;

    /**
     * Removes this node.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @throws KException
     *         if an error occurs
     */
    void remove( final UnitOfWork transaction ) throws KException;

    /**
     * To remove children with same name, the same name must be passed in more than once. It is recommended to use
     * {@link #remove(UnitOfWork)} whenever the node being removed is available.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
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
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @param descriptorNames
     *        the mixin(s) being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeDescriptor( final UnitOfWork transaction,
                           final String... descriptorNames ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @param newName
     *        the new name (cannot be empty or contain any slashes)
     * @throws KException
     *         if an error occurs
     */
    void rename( final UnitOfWork transaction,
                 final String newName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
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
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
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
     * Visit this object with the given visitor.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @param visitor
     *        the visitor
     * @throws Exception
     *         if error occurs
     */
    void visit( final UnitOfWork transaction,
                final KomodoObjectVisitor visitor ) throws Exception;

}
