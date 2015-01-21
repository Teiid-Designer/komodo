/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.internal.vdb;

import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Permission;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * An implementation of a VDB data role.
 */
public final class DataRoleImpl extends RelationalObjectImpl implements DataRole {

    /**
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public DataRoleImpl( final Repository repository,
                         final String workspacePath ) throws KException {
        super(repository, workspacePath);
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
            LOGGER.debug("addMappedRole: transaction = '{0}', roleNameToAdd = '{1}'", //$NON-NLS-1$
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
            LOGGER.debug("addPermission: transaction = '{0}', permissionName = '{1}'", //$NON-NLS-1$
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
     * @see org.komodo.relational.vdb.DataRole#getDescription(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDescription( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("dataroleimpl-getDescription", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            String result = null;
            final Property property = getProperty(transaction, VdbLexicon.DataRole.DESCRIPTION);

            if (property != null) {
                result = property.getStringValue();
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
                result = property.getStringValues();
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
            LOGGER.debug("getPermissions: transaction = '{0}'", transaction.getName()); //$NON-NLS-1$
        }

        try {
            Permission[] result = null;

            if (hasChild(transaction, VdbLexicon.DataRole.PERMISSIONS)) {
                final KomodoObject grouping = getChild(transaction, VdbLexicon.DataRole.PERMISSIONS);
                final List< Permission > temp = new ArrayList<>();

                for (final KomodoObject kobject : grouping.getChildrenOfType(transaction,
                                                                             VdbLexicon.DataRole.Permission.PERMISSION)) {
                    final Permission permission = new PermissionImpl(getRepository(), kobject.getAbsolutePath());

                    if (LOGGER.isDebugEnabled()) {
                        LOGGER.debug("getPermissions: transaction = '{0}', found permission = '{1}'", //$NON-NLS-1$
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
     * @see org.komodo.relational.vdb.DataRole#isAllowCreateTempTables(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAllowCreateTempTables( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("dataroleimpl-isAllowCreateTempTables", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            boolean result = DataRole.DEFAULT_ALLOW_CREATE_TEMP_TABLES;
            final Property property = getProperty(transaction, VdbLexicon.DataRole.ALLOW_CREATE_TEMP_TABLES);

            if (property != null) {
                result = property.getBooleanValue();
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
     * @see org.komodo.relational.vdb.DataRole#isAnyAuthenticated(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAnyAuthenticated( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("dataroleimpl-isAnyAuthenticated", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            boolean result = DataRole.DEFAULT_ANY_AUTHENTICATED;
            final Property property = getProperty(transaction, VdbLexicon.DataRole.ANY_AUTHENTICATED);

            if (property != null) {
                result = property.getBooleanValue();
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
     * @see org.komodo.relational.vdb.DataRole#isGrantAll(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isGrantAll( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("dataroleimpl-isGrantAll", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            boolean result = DataRole.DEFAULT_GRANT_ALL;
            final Property property = getProperty(transaction, VdbLexicon.DataRole.GRANT_ALL);

            if (property != null) {
                result = property.getBooleanValue();
            }

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

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
            LOGGER.debug("removeMappedRole: transaction = '{0}', roleNameToRemove = '{1}'", //$NON-NLS-1$
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
            LOGGER.debug("removePermission: transaction = '{0}', permissionToRemove = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         permissionToRemove);
        }

        boolean found = false;

        try {
            if (hasChild(transaction, VdbLexicon.DataRole.PERMISSIONS)) {
                final KomodoObject grouping = getChild(transaction, VdbLexicon.DataRole.PERMISSIONS);

                if (grouping.hasChild(transaction, permissionToRemove)) {
                    grouping.removeChild(transaction, permissionToRemove);
                    found = true;
                }
            }

            if (!found) {
                throw new KException(Messages.getString(Relational.PERMISSION_NOT_FOUND_TO_REMOVE, permissionToRemove));
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
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("dataroleimpl-setAllowCreateTempTables", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setAllowCreateTempTables: transaction = '{0}', newAllowCreateTempTables = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newAllowCreateTempTables);
        }

        try {
            setProperty(transaction, VdbLexicon.DataRole.ALLOW_CREATE_TEMP_TABLES, newAllowCreateTempTables);

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
     * @see org.komodo.relational.vdb.DataRole#setAnyAuthenticated(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAnyAuthenticated( final UnitOfWork uow,
                                     final boolean newAnyAuthenticated ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("dataroleimpl-setAnyAuthenticated", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setAnyAuthenticated: transaction = '{0}', newAnyAuthenticated = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newAnyAuthenticated);
        }

        try {
            setProperty(transaction, VdbLexicon.DataRole.ANY_AUTHENTICATED, newAnyAuthenticated);

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
     * @see org.komodo.relational.vdb.DataRole#setDescription(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setDescription( final UnitOfWork uow,
                                final String newDescription ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("dataroleimpl-setDescription", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setDescription: transaction = '{0}', newDescription = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newDescription);
        }

        try {
            setProperty(transaction, VdbLexicon.DataRole.DESCRIPTION, StringUtils.isBlank(newDescription) ? null : newDescription);

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
     * @see org.komodo.relational.vdb.DataRole#setGrantAll(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setGrantAll( final UnitOfWork uow,
                             final boolean newGrantAll ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("dataroleimpl-setGrantAll", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setGrantAll: transaction = '{0}', newGrantAll = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newGrantAll);
        }

        try {
            setProperty(transaction, VdbLexicon.DataRole.GRANT_ALL, newGrantAll);

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

}
