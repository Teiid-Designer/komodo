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
     * Name patterns may be a full name or a partial name with one or more wildcard characters ("*"). If a node name matches any
     * of the patterns, it is returned.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or must have a state of {@link State#NOT_STARTED}))
     * @param namePatterns
     *        optional name patterns of the child(ren) being requested (can be <code>null</code> or empty but cannot have
     *        <code>null</code> or empty elements)
     * @return the child object(s) (never <code>null</code> but can be empty)
     * @throws KException
     *         if the child does not exist or an error occurs
     */
    KomodoObject[] getChildren( final UnitOfWork transaction,
                                final String... namePatterns ) throws KException;

    /**
     * Name patterns may be a full name or a partial name with one or more wildcard characters ("*"). If a node name matches any
     * of the patterns, it is returned.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @param type
     *        the primary type or mixin of the children being requested (cannot be empty)
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the matching children (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    KomodoObject[] getChildrenOfType( final UnitOfWork transaction,
                                      final String type,
                                      final String... namePatterns ) throws KException;

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
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @return the property descriptors from the primary type descriptor and the mixin descriptors (never <code>null</code> but
     *         can be empty)
     * @throws KException
     *         if an error occurs
     */
    PropertyDescriptor[] getPropertyDescriptors( final UnitOfWork transaction ) throws KException;

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
     * Subclasses may implement {@link #getChildren(UnitOfWork, String...)} in such a way that it does not represent the actual
     * set of child nodes. This method obtains the actual child nodes.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @param namePatterns
     *        optional name patterns of the child(ren) being requested (can be <code>null</code> or empty but cannot have
     *        <code>null</code> or empty elements)
     * @return the unfiltered child objects (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    KomodoObject[] getRawChildren( final UnitOfWork transaction,
                                   final String... namePatterns ) throws KException;

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
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED}
     * @return the physical parent {@link KomodoObject Komodo object} (can be <code>null</code> if at the Komodo root)
     * @throws KException
     *         if an error occurs
     */
    KomodoObject getRawParent( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @return the unfiltered property descriptors from the primary type descriptor and the mixin descriptors (never <code>null</code> but
     *         can be empty)
     * @throws KException
     *         if an error occurs
     */
    PropertyDescriptor[] getRawPropertyDescriptors( final UnitOfWork transaction ) throws KException;

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
     * Indicates if a child exists with the specified name, regardless of the type. This method can be overridden by subclasses to
     * filter out child nodes.
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
     * Indicates if a child exists with the specified name and the specified primary type or mixin. This method can be overridden
     * by subclasses to filter out child nodes.
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
     * This method can be overridden by subclasses to filter out child nodes.
     *
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
     * Indicates if a child, filtered or unfiltered, exists with the specified name, regardless of the type.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state other than {@link State#NOT_STARTED}))
     * @param name
     *        the name of the child whose existence is being checked (cannot be empty)
     * @return <code>true</code> if a child with the supplied name exists
     * @throws KException
     *         if an error occurs
     */
    boolean hasRawChild( final UnitOfWork transaction,
                         final String name ) throws KException;

    /**
     * Indicates if a child, filtered or unfiltered, exists with the specified name and the specified primary type or mixin.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state other than {@link State#NOT_STARTED}))
     * @param name
     *        the name of the child whose existence is being checked (cannot be empty)
     * @param typeName
     *        the primary type or mixin (cannot be empty)
     * @return <code>true</code> if a child with the supplied name and type exists
     * @throws KException
     *         if an error occurs
     */
    boolean hasRawChild( final UnitOfWork transaction,
                         final String name,
                         final String typeName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state other than {@link State#NOT_STARTED}))
     * @return <code>true</code> if children exist
     * @throws KException
     *         if an error occurs
     */
    boolean hasRawChildren( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @param name
     *        the name the property, filtered or unfiltered, whose existence is being checked (cannot be empty)
     * @return <code>true</code> if a property with the supplied name exists
     * @throws KException
     *         if an error occurs
     */
    boolean hasRawProperty( final UnitOfWork transaction,
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
    void accept( final UnitOfWork transaction,
                final KomodoObjectVisitor visitor ) throws Exception;

    /**
     * @return the object's zero-based index relative to any other same-name-siblings or -1 if there is no same-name-siblings
     */
    int getIndex();

}
