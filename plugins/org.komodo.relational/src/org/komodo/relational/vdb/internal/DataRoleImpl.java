/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.vdb.internal;

import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Vdb;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * An implementation of a VDB data role.
 */
public final class DataRoleImpl extends RelationalObjectImpl implements DataRole {

    /**
     * The resolver of a {@link DataRole}.
     */
    public static final TypeResolver< DataRole > RESOLVER = new TypeResolver< DataRole >() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public DataRole create( final UnitOfWork transaction,
                                final Repository repository,
                                final KomodoObject parent,
                                final String id,
                                final RelationalProperties properties ) throws KException {
            final AdapterFactory adapter = new AdapterFactory( repository );
            final Vdb parentVdb = adapter.adapt( transaction, parent, Vdb.class );
            return RelationalModelFactory.createDataRole( transaction, repository, parentVdb, id );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#identifier()
         */
        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#owningClass()
         */
        @Override
        public Class< DataRoleImpl > owningClass() {
            return DataRoleImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final KomodoObject kobject ) {
            try {
                ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, VdbLexicon.DataRole.DATA_ROLE );
                return true;
            } catch (final Exception e) {
                // not resolvable
            }

            return false;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public DataRole resolve( final UnitOfWork transaction,
                                 final KomodoObject kobject ) throws KException {
            return new DataRoleImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
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
        return KomodoType.VDB_DATA_ROLE;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.DataRole#addMappedRole(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public String[] addMappedRole( final UnitOfWork uow,
                                   final String roleNameToAdd ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("dataroleimpl-addMappedRole", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("addMappedRole: transaction = {0}, roleNameToAdd = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         roleNameToAdd);
        }

        String[] result = null;

        try {
            ArgCheck.isNotEmpty(roleNameToAdd, "roleNameToAdd"); //$NON-NLS-1$
            final String[] current = getMappedRoles(transaction);
            int i = 0;

            if (current.length == 0) {
                // this is first mapped role name
                result = new String[1];
            } else {
                // add to existing (make sure it doesn't already exist)
                result = new String[current.length + 1];

                for (final String mappedRoleName : current) {
                    if (mappedRoleName.equals(roleNameToAdd)) {
                        throw new KException(Messages.getString(Relational.DUPLICATE_ROLE_NAME, roleNameToAdd));
                    }

                    result[i++] = mappedRoleName;
                }
            }

            result[i] = roleNameToAdd;
            setProperty(transaction, VdbLexicon.DataRole.MAPPED_ROLE_NAMES, (Object[])result);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.DataRole#addPermission(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Permission addPermission( final UnitOfWork uow,
                                     final String permissionName ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("dataroleimpl-addPermission", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("addPermission: transaction = {0}, permissionName = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         permissionName);
        }

        try {
            ArgCheck.isNotEmpty(permissionName, "permissionName"); //$NON-NLS-1$
            final Permission result = RelationalModelFactory.createPermission(transaction, getRepository(), this, permissionName);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoObject[] getChildren( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = getRepository().createTransaction("dataroleimpl-getChildren", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject[] result = getPermissions(transaction);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildrenOfType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public KomodoObject[] getChildrenOfType( final UnitOfWork uow,
                                             final String type ) throws KException {
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = getRepository().createTransaction("dataroleimpl-getChildrenOfType", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            KomodoObject[] result = null;

            if (VdbLexicon.DataRole.Permission.PERMISSION.equals(type)) {
                result = getPermissions(transaction);
            } else {
                result = KomodoObject.EMPTY_ARRAY;
            }

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
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
     * @see org.komodo.relational.vdb.DataRole#getMappedRoles(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String[] getMappedRoles( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("dataroleimpl-getMappedRoleNames", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        String[] result = null;

        try {
            final Property property = getProperty(transaction, VdbLexicon.DataRole.MAPPED_ROLE_NAMES);

            if (property == null) {
                // this is first mapped role name
                result = StringConstants.EMPTY_ARRAY;
            } else {
                result = property.getStringValues(transaction);
            }

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoObject getParent( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("dataroleimpl-getParent", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject grouping = super.getParent(transaction);
            final KomodoObject result = resolveType(transaction, grouping.getParent(transaction));

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.DataRole#getPermissions(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Permission[] getPermissions( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("dataroleimpl-getPermissions", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("getPermissions: transaction = {0}", transaction.getName()); //$NON-NLS-1$
        }

        try {
            Permission[] result = null;

            if (hasChild(transaction, VdbLexicon.DataRole.PERMISSIONS, VdbLexicon.DataRole.PERMISSIONS)) {
                final KomodoObject grouping = getChild( transaction,
                                                        VdbLexicon.DataRole.PERMISSIONS,
                                                        VdbLexicon.DataRole.PERMISSIONS );
                final List< Permission > temp = new ArrayList<>();

                for (final KomodoObject kobject : grouping.getChildrenOfType(transaction,
                                                                             VdbLexicon.DataRole.Permission.PERMISSION)) {
                    final Permission permission = new PermissionImpl(transaction, getRepository(), kobject.getAbsolutePath());

                    if (LOGGER.isDebugEnabled()) {
                        LOGGER.debug("getPermissions: transaction = {0}, found permission = {1}", //$NON-NLS-1$
                                     transaction.getName(),
                                     kobject.getAbsolutePath());
                    }

                    temp.add(permission);
                }

                result = temp.toArray(new Permission[temp.size()]);
            } else {
                result = Permission.NO_PERMISSIONS;
            }

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
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
    public String[] removeMappedRole( final UnitOfWork uow,
                                      final String roleNameToRemove ) throws KException {
        ArgCheck.isNotEmpty(roleNameToRemove, "roleNameToRemove"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("dataroleimpl-removeMappedRole", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removeMappedRole: transaction = {0}, roleNameToRemove = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         roleNameToRemove);
        }

        try {
            final String[] current = getMappedRoles(transaction);

            if (current.length == 0) {
                throw new KException(Messages.getString(Relational.MAPPED_ROLE_NOT_FOUND_TO_REMOVE, roleNameToRemove));
            }

            final String[] result = new String[current.length - 1];
            boolean found = false;
            int i = 0;

            for (final String mappedRoleName : current) {
                if (mappedRoleName.equals(roleNameToRemove)) {
                    found = true;
                } else {
                    result[i++] = mappedRoleName;
                }
            }

            if (!found) {
                throw new KException(Messages.getString(Relational.MAPPED_ROLE_NOT_FOUND_TO_REMOVE, roleNameToRemove));
            }

            final Object[] newValue = ((result.length == 0) ? null : result);
            setProperty(transaction, VdbLexicon.DataRole.MAPPED_ROLE_NAMES, newValue);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.DataRole#removePermission(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removePermission( final UnitOfWork uow,
                                  final String permissionToRemove ) throws KException {
        ArgCheck.isNotEmpty(permissionToRemove, "permissionToRemove"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("dataroleimpl-removePermission", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removePermission: transaction = {0}, permissionToRemove = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         permissionToRemove);
        }

        boolean found = false;

        try {
            final Permission[] permissions = getPermissions( transaction );

            if (permissions.length != 0) {
                for (final Permission permission : permissions) {
                    if (permissionToRemove.equals( permission.getName( transaction ) )) {
                        permission.remove( transaction );
                        found = true;
                        break;
                    }
                }
            }

            if (!found) {
                throw new KException( Messages.getString( Relational.PERMISSION_NOT_FOUND_TO_REMOVE, permissionToRemove ) );
            }

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
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
