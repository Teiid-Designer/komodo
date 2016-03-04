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
package org.komodo.relational.vdb.internal;

import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * An implementation of a VDB data role.
 */
public final class DataRoleImpl extends RelationalObjectImpl implements DataRole {

    /**
     * The allowed child types.
     */
    private static final KomodoType[] CHILD_TYPES = new KomodoType[] { Permission.IDENTIFIER };

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public DataRoleImpl( final UnitOfWork uow,
                         final Repository repository,
                         final String workspacePath ) throws KException {
        super(uow, repository, workspacePath);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return DataRole.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.DataRole#addMappedRole(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public String[] addMappedRole( final UnitOfWork transaction,
                                   final String roleNameToAdd ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( roleNameToAdd, "roleNameToAdd" ); //$NON-NLS-1$

        String[] result = null;
        final String[] current = getMappedRoles( transaction );
        int i = 0;

        if ( current.length == 0 ) {
            // this is first mapped role name
            result = new String[ 1 ];
        } else {
            // add to existing (make sure it doesn't already exist)
            result = new String[ current.length + 1 ];

            for ( final String mappedRoleName : current ) {
                if ( mappedRoleName.equals( roleNameToAdd ) ) {
                    throw new KException( Messages.getString( Relational.DUPLICATE_ROLE_NAME, roleNameToAdd ) );
                }

                result[i++] = mappedRoleName;
            }
        }

        result[i] = roleNameToAdd;
        setProperty( transaction, VdbLexicon.DataRole.MAPPED_ROLE_NAMES, ( Object[] )result );

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.DataRole#addPermission(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Permission addPermission( final UnitOfWork transaction,
                                     final String permissionName ) throws KException {
        return RelationalModelFactory.createPermission( transaction, getRepository(), this, permissionName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public KomodoObject getChild( final UnitOfWork transaction,
                                  final String name ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$

        final KomodoObject[] matches = getPermissions( transaction, name );

        if ( matches.length != 0 ) {
            return matches[ 0 ];
        }

        // child does not exist
        throw new KException( Messages.getString( org.komodo.repository.Messages.Komodo.CHILD_NOT_FOUND,
                                                  name,
                                                  getAbsolutePath() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String, java.lang.String)
     */
    @Override
    public KomodoObject getChild( final UnitOfWork transaction,
                                  final String name,
                                  final String typeName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( typeName, "typeName" ); //$NON-NLS-1$

        if ( VdbLexicon.DataRole.Permission.PERMISSION.equals( typeName ) ) {
            final KomodoObject[] permissions = getPermissions( transaction, name );

            if ( permissions.length != 0 ) {
                return permissions[ 0 ];
            }
        }

        // child does not exist
        throw new KException( Messages.getString( org.komodo.repository.Messages.Komodo.CHILD_NOT_FOUND,
                                                  name,
                                                  getAbsolutePath() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildren(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public KomodoObject[] getChildren( final UnitOfWork transaction,
                                       final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject[] result = getPermissions( transaction, namePatterns );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildrenOfType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String[])
     */
    @Override
    public KomodoObject[] getChildrenOfType( final UnitOfWork transaction,
                                             final String type,
                                             final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        KomodoObject[] result = null;

        if ( VdbLexicon.DataRole.Permission.PERMISSION.equals( type ) ) {
            result = getPermissions( transaction, namePatterns );
        } else {
            result = KomodoObject.EMPTY_ARRAY;
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildTypes()
     */
    @Override
    public KomodoType[] getChildTypes() {
        return CHILD_TYPES;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.DataRole#getDescription(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDescription( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.STRING, "getDescription", VdbLexicon.DataRole.DESCRIPTION); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.DataRole#getMappedRoles(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public String[] getMappedRoles( final UnitOfWork transaction,
                                    final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final Property property = getProperty( transaction, VdbLexicon.DataRole.MAPPED_ROLE_NAMES );

        if ( property == null ) {
            return StringConstants.EMPTY_ARRAY;
        }

        final boolean matchPattern = ( ( namePatterns != null ) && ( namePatterns.length != 0 ) );
        final List< String > roleNames = new ArrayList< >();

        for ( final String value : property.getStringValues( transaction ) ) {
            if ( matchPattern ) {
                for ( final String pattern : namePatterns ) {
                    // convert pattern to a regex
                    final String regex = pattern.replace( "*", ".*" ); //$NON-NLS-1$ //$NON-NLS-2$

                    if ( value.matches( regex ) ) {
                        roleNames.add( value );
                    }
                }
            } else {
                roleNames.add( value );
            }
        }

        return roleNames.toArray( new String[ roleNames.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Vdb getParent( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject grouping = super.getParent( transaction );
        final Vdb result = Vdb.RESOLVER.resolve( transaction, grouping.getParent( transaction ) );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.DataRole#getPermissions(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public Permission[] getPermissions( final UnitOfWork transaction,
                                        final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject grouping = getPermissionsGroupingNode( transaction );

        if ( grouping != null ) {
            final List< Permission > temp = new ArrayList<>();

            for ( final KomodoObject kobject : grouping.getChildren( transaction, namePatterns ) ) {
                final Permission permission = new PermissionImpl( transaction, getRepository(), kobject.getAbsolutePath() );
                temp.add( permission );
            }

            return temp.toArray( new Permission[ temp.size() ] );
        }

        return Permission.NO_PERMISSIONS;
    }

    private KomodoObject getPermissionsGroupingNode( final UnitOfWork transaction ) {
        try {
            final KomodoObject[] groupings = getRawChildren( transaction, VdbLexicon.DataRole.PERMISSIONS );

            if ( groupings.length == 0 ) {
                return null;
            }

            return groupings[ 0 ];
        } catch ( final KException e ) {
            return null;
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getTypeId()
     */
    @Override
    public int getTypeId() {
        return TYPE_ID;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChild(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public boolean hasChild( final UnitOfWork transaction,
                             final String name ) throws KException {
        if ( VdbLexicon.DataRole.PERMISSIONS.equals( name ) ) {
            return false; // use hasRawChild
        }

        return ( super.hasChild( transaction, name ) || ( getPermissions( transaction, name ).length != 0 ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChild(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String)
     */
    @Override
    public boolean hasChild( final UnitOfWork transaction,
                             final String name,
                             final String typeName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( typeName, "typeName" ); //$NON-NLS-1$

        if ( VdbLexicon.DataRole.Permission.PERMISSION.equals( typeName ) ) {
            return ( getPermissions( transaction, name ).length != 0 );
        }

        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean hasChildren( final UnitOfWork transaction ) throws KException {
        // short-circuit with call to super (will also return the permissions grouping node)
        // call to getChildren does not return source grouping node
        return ( super.hasChildren( transaction ) && ( getChildren( transaction ).length != 0 ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.DataRole#isAllowCreateTempTables(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAllowCreateTempTables( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.BOOLEAN, "isAllowCreateTempTables", //$NON-NLS-1$
                                 VdbLexicon.DataRole.ALLOW_CREATE_TEMP_TABLES);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.DataRole#isAnyAuthenticated(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAnyAuthenticated( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.BOOLEAN, "isAnyAuthenticated", VdbLexicon.DataRole.ANY_AUTHENTICATED); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.DataRole#isGrantAll(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isGrantAll( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.BOOLEAN, "isAnyAuthenticated", VdbLexicon.DataRole.GRANT_ALL); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.DataRole#removeMappedRole(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public String[] removeMappedRole( final UnitOfWork transaction,
                                      final String roleNameToRemove ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( roleNameToRemove, "roleNameToRemove" ); //$NON-NLS-1$

        final String[] current = getMappedRoles( transaction );

        if ( current.length == 0 ) {
            throw new KException( Messages.getString( Relational.MAPPED_ROLE_NOT_FOUND_TO_REMOVE, roleNameToRemove ) );
        }

        final String[] result = new String[ current.length - 1 ];
        boolean found = false;
        int i = 0;

        for ( final String mappedRoleName : current ) {
            if ( mappedRoleName.equals( roleNameToRemove ) ) {
                found = true;
            } else {
                result[i++] = mappedRoleName;
            }
        }

        if ( !found ) {
            throw new KException( Messages.getString( Relational.MAPPED_ROLE_NOT_FOUND_TO_REMOVE, roleNameToRemove ) );
        }

        final Object[] newValue = ( ( result.length == 0 ) ? null : result );
        setProperty( transaction, VdbLexicon.DataRole.MAPPED_ROLE_NAMES, newValue );

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.DataRole#removePermission(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removePermission( final UnitOfWork transaction,
                                  final String permissionToRemove ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( permissionToRemove, "permissionToRemove" ); //$NON-NLS-1$

        final Permission[] permissions = getPermissions( transaction, permissionToRemove );

        if ( permissions.length == 0 ) {
            throw new KException( Messages.getString( Relational.PERMISSION_NOT_FOUND_TO_REMOVE, permissionToRemove ) );
        }

        // remove first occurrence
        permissions[ 0 ].remove( transaction );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.DataRole#setAllowCreateTempTables(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAllowCreateTempTables( final UnitOfWork uow,
                                          final boolean newAllowCreateTempTables ) throws KException {
        setObjectProperty(uow, "setAllowCreateTempTables", VdbLexicon.DataRole.ALLOW_CREATE_TEMP_TABLES, newAllowCreateTempTables); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.DataRole#setAnyAuthenticated(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAnyAuthenticated( final UnitOfWork uow,
                                     final boolean newAnyAuthenticated ) throws KException {
        setObjectProperty(uow, "setAnyAuthenticated", VdbLexicon.DataRole.ANY_AUTHENTICATED, newAnyAuthenticated); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.DataRole#setDescription(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setDescription( final UnitOfWork uow,
                                final String newDescription ) throws KException {
        setObjectProperty(uow, "setDescription", VdbLexicon.DataRole.DESCRIPTION, newDescription); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.DataRole#setGrantAll(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setGrantAll( final UnitOfWork uow,
                             final boolean newGrantAll ) throws KException {
        setObjectProperty(uow, "setGrantAll", VdbLexicon.DataRole.GRANT_ALL, newGrantAll); //$NON-NLS-1$
    }

}
