/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.vdb;

import org.komodo.relational.model.RelationalObject;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * Represents a VDB data role.
 */
public interface DataRole extends RelationalObject {

    /**
     * The default value for the <code>create temporary tables</code> property. Value is {@value} .
     */
    boolean DEFAULT_ALLOW_CREATE_TEMP_TABLES = false;

    /**
     * The default value for the <code>any authenticated</code> property. Value is {@value} .
     */
    boolean DEFAULT_ANY_AUTHENTICATED = false;

    /**
     * The default value for the <code>grant all</code> property. Value is {@value} .
     */
    boolean DEFAULT_GRANT_ALL = false;

    /**
     * An empty array of data roles.
     */
    DataRole[] NO_DATA_ROLES = new DataRole[0];

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param roleName
     *        the name of the mapped role being added (cannot be empty)
     * @return all the mapped roles (never empty)
     * @throws KException
     *         if an error occurs
     */
    String[] addMappedRole( final UnitOfWork transaction,
                            final String roleName ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param permissionName
     *        the name of the permission being added (cannot be empty)
     * @return the new permission (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Permission addPermission( final UnitOfWork transaction,
                              final String permissionName ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>description</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getDescription( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @return the mapped role names (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    String[] getMappedRoles( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the permissions (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Permission[] getPermissions( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if allows creating temporary tables
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOW_CREATE_TEMP_TABLES
     */
    boolean isAllowCreateTempTables( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if any authenticated
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ANY_AUTHENTICATED
     */
    boolean isAnyAuthenticated( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if grant all
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_GRANT_ALL
     */
    boolean isGrantAll( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param roleNameToRemove
     *        the name of the role being removed (cannot be empty)
     * @return all the mapped roles (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    String[] removeMappedRole( final UnitOfWork transaction,
                               final String roleNameToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param permissionToRemove
     *        the name of the permission being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removePermission( final UnitOfWork transaction,
                           final String permissionToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newAllowCreateTempTables
     *        the new value for the <code>allow creating temporary tables</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOW_CREATE_TEMP_TABLES
     */
    void setAllowCreateTempTables( final UnitOfWork transaction,
                                   final boolean newAllowCreateTempTables ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newAnyAuthenticated
     *        the new value for the <code>any authenticated</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ANY_AUTHENTICATED
     */
    void setAnyAuthenticated( final UnitOfWork transaction,
                              final boolean newAnyAuthenticated ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newDescription
     *        the new value of the <code>description</code> property
     * @throws KException
     *         if an error occurs
     */
    void setDescription( final UnitOfWork transaction,
                         final String newDescription ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newGrantAll
     *        the new value for the <code>grant all</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_GRANT_ALL
     */
    void setGrantAll( final UnitOfWork transaction,
                      final boolean newGrantAll ) throws KException;

}
