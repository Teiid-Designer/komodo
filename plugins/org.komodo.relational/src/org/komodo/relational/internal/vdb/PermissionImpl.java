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
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.Mask;
import org.komodo.relational.vdb.Permission;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * An implementation of a VDB data policy permission.
 */
public final class PermissionImpl extends RelationalObjectImpl implements Permission {

    /**
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public PermissionImpl( final Repository repository,
                           final String workspacePath ) throws KException {
        super(repository, workspacePath);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Permission#addCondition(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Condition addCondition( final UnitOfWork uow,
                                   final String conditionName ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("permissionimpl-addCondition", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("addCondition: transaction = '{0}', conditionName = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         conditionName);
        }

        try {
            ArgCheck.isNotEmpty(conditionName, "conditionName"); //$NON-NLS-1$
            final Condition result = RelationalModelFactory.createCondition(transaction, getRepository(), this, conditionName);

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
     * @see org.komodo.relational.vdb.Permission#addMask(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Mask addMask( final UnitOfWork uow,
                         final String maskName ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("permissionimpl-addMask", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("addMask: transaction = '{0}', maskName = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         maskName);
        }

        try {
            ArgCheck.isNotEmpty(maskName, "maskName"); //$NON-NLS-1$
            final Mask result = RelationalModelFactory.createMask(transaction, getRepository(), this, maskName);

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
     * @see org.komodo.relational.vdb.Permission#getConditions(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Condition[] getConditions( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("permissionimpl-getConditions", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("getConditions: transaction = '{0}'", transaction.getName()); //$NON-NLS-1$
        }

        try {
            Condition[] result = null;

            if (hasChild(transaction, VdbLexicon.DataRole.Permission.CONDITIONS)) {
                final KomodoObject grouping = getChild(transaction, VdbLexicon.DataRole.Permission.CONDITIONS);
                final List< Condition > temp = new ArrayList<>();

                for (final KomodoObject kobject : grouping.getChildrenOfType(transaction,
                                                                             VdbLexicon.DataRole.Permission.Condition.CONDITION)) {
                    final Condition condition = new ConditionImpl(getRepository(), kobject.getAbsolutePath());

                    if (LOGGER.isDebugEnabled()) {
                        LOGGER.debug("getConditions: transaction = '{0}', found condition = '{1}'", //$NON-NLS-1$
                                     transaction.getName(),
                                     kobject.getAbsolutePath());
                    }

                    temp.add(condition);
                }

                result = temp.toArray(new Condition[temp.size()]);
            } else {
                result = Condition.NO_CONDITIONS;
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
     * @see org.komodo.relational.vdb.Permission#getMasks(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Mask[] getMasks( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("permissionimpl-getMasks", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("getMasks: transaction = '{0}'", transaction.getName()); //$NON-NLS-1$
        }

        try {
            Mask[] result = null;

            if (hasChild(transaction, VdbLexicon.DataRole.Permission.MASKS)) {
                final KomodoObject grouping = getChild(transaction, VdbLexicon.DataRole.Permission.MASKS);
                final List< Mask > temp = new ArrayList<>();

                for (final KomodoObject kobject : grouping.getChildrenOfType(transaction,
                                                                             VdbLexicon.DataRole.Permission.Mask.MASK)) {
                    final Mask mask = new MaskImpl(getRepository(), kobject.getAbsolutePath());

                    if (LOGGER.isDebugEnabled()) {
                        LOGGER.debug("getMasks: transaction = '{0}', found mask = '{1}'", //$NON-NLS-1$
                                     transaction.getName(),
                                     kobject.getAbsolutePath());
                    }

                    temp.add(mask);
                }

                result = temp.toArray(new Mask[temp.size()]);
            } else {
                result = Mask.NO_MASKS;
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
     * @see org.komodo.relational.vdb.Permission#isAllowAlter(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAllowAlter( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("permissionimpl-isAllowAlter", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            boolean result = Permission.DEFAULT_ALLOW_ALTER;
            final Property property = getProperty(transaction, VdbLexicon.DataRole.Permission.ALLOW_ALTER);

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
     * @see org.komodo.relational.vdb.Permission#isAllowCreate(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAllowCreate( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("permissionimpl-isAllowCreate", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            boolean result = Permission.DEFAULT_ALLOW_CREATE;
            final Property property = getProperty(transaction, VdbLexicon.DataRole.Permission.ALLOW_CREATE);

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
     * @see org.komodo.relational.vdb.Permission#isAllowDelete(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAllowDelete( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("permissionimpl-isAllowDelete", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            boolean result = Permission.DEFAULT_ALLOW_DELETE;
            final Property property = getProperty(transaction, VdbLexicon.DataRole.Permission.ALLOW_DELETE);

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
     * @see org.komodo.relational.vdb.Permission#isAllowExecute(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAllowExecute( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("permissionimpl-isAllowExecute", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            boolean result = Permission.DEFAULT_ALLOW_EXECUTE;
            final Property property = getProperty(transaction, VdbLexicon.DataRole.Permission.ALLOW_EXECUTE);

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
     * @see org.komodo.relational.vdb.Permission#isAllowLanguage(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAllowLanguage( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("permissionimpl-isAllowLanguage", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            boolean result = Permission.DEFAULT_ALLOW_LANGUAGE;
            final Property property = getProperty(transaction, VdbLexicon.DataRole.Permission.ALLOW_LANGUAGE);

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
     * @see org.komodo.relational.vdb.Permission#isAllowRead(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAllowRead( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("permissionimpl-isAllowRead", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            boolean result = Permission.DEFAULT_ALLOW_READ;
            final Property property = getProperty(transaction, VdbLexicon.DataRole.Permission.ALLOW_READ);

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
     * @see org.komodo.relational.vdb.Permission#isAllowUpdate(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAllowUpdate( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("permissionimpl-isAllowUpdate", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            boolean result = Permission.DEFAULT_ALLOW_UPDATE;
            final Property property = getProperty(transaction, VdbLexicon.DataRole.Permission.ALLOW_UPDATE);

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
     * @see org.komodo.relational.vdb.Permission#removeCondition(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void removeCondition( final UnitOfWork uow,
                                 final String conditionToRemove ) throws KException {
        ArgCheck.isNotEmpty(conditionToRemove, "conditionToRemove"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("permissionimpl-removeCondition", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removeCondition: transaction = '{0}', conditionToRemove = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         conditionToRemove);
        }

        boolean found = false;

        try {
            if (hasChild(transaction, VdbLexicon.DataRole.Permission.CONDITIONS)) {
                final KomodoObject grouping = getChild(transaction, VdbLexicon.DataRole.Permission.CONDITIONS);

                if (grouping.hasChild(transaction, conditionToRemove)) {
                    grouping.removeChild(transaction, conditionToRemove);
                    found = true;
                }
            }

            if (!found) {
                throw new KException(Messages.getString(Relational.CONDITION_NOT_FOUND_TO_REMOVE, conditionToRemove));
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
     * @see org.komodo.relational.vdb.Permission#removeMask(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeMask( final UnitOfWork uow,
                            final String maskToRemove ) throws KException {
        ArgCheck.isNotEmpty(maskToRemove, "maskToRemove"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("permissionimpl-removeMask", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("removeMask: transaction = '{0}', maskToRemove = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         maskToRemove);
        }

        boolean found = false;

        try {
            if (hasChild(transaction, VdbLexicon.DataRole.Permission.MASKS)) {
                final KomodoObject grouping = getChild(transaction, VdbLexicon.DataRole.Permission.MASKS);

                if (grouping.hasChild(transaction, maskToRemove)) {
                    grouping.removeChild(transaction, maskToRemove);
                    found = true;
                }
            }

            if (!found) {
                throw new KException(Messages.getString(Relational.MASK_NOT_FOUND_TO_REMOVE, maskToRemove));
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
     * @see org.komodo.relational.vdb.Permission#setAllowAlter(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAllowAlter( final UnitOfWork uow,
                               final boolean newAllowAlter ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("permissionimpl-setAllowAlter", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setAllowAlter: transaction = '{0}', newAllowAlter = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newAllowAlter);
        }

        try {
            setProperty(transaction, VdbLexicon.DataRole.Permission.ALLOW_ALTER, newAllowAlter);

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
     * @see org.komodo.relational.vdb.Permission#setAllowCreate(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAllowCreate( final UnitOfWork uow,
                                final boolean newAllowCreate ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("permissionimpl-setAllowCreate", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setAllowCreate: transaction = '{0}', newAllowCreate = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newAllowCreate);
        }

        try {
            setProperty(transaction, VdbLexicon.DataRole.Permission.ALLOW_CREATE, newAllowCreate);

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
     * @see org.komodo.relational.vdb.Permission#setAllowDelete(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAllowDelete( final UnitOfWork uow,
                                final boolean newAllowDelete ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("permissionimpl-setAllowDelete", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setAllowDelete: transaction = '{0}', newAllowDelete = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newAllowDelete);
        }

        try {
            setProperty(transaction, VdbLexicon.DataRole.Permission.ALLOW_DELETE, newAllowDelete);

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
     * @see org.komodo.relational.vdb.Permission#setAllowExecute(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAllowExecute( final UnitOfWork uow,
                                 final boolean newAllowExecute ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("permissionimpl-setAllowExecute", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setAllowExecute: transaction = '{0}', newAllowExecute = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newAllowExecute);
        }

        try {
            setProperty(transaction, VdbLexicon.DataRole.Permission.ALLOW_EXECUTE, newAllowExecute);

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
     * @see org.komodo.relational.vdb.Permission#setAllowLanguage(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAllowLanguage( final UnitOfWork uow,
                                  final boolean newAllowLanguage ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("permissionimpl-setAllowLanguage", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setAllowLanguage: transaction = '{0}', newAllowLanguage = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newAllowLanguage);
        }

        try {
            setProperty(transaction, VdbLexicon.DataRole.Permission.ALLOW_LANGUAGE, newAllowLanguage);

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
     * @see org.komodo.relational.vdb.Permission#setAllowRead(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAllowRead( final UnitOfWork uow,
                              final boolean newAllowRead ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("permissionimpl-setAllowRead", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setAllowRead: transaction = '{0}', newAllowRead = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newAllowRead);
        }

        try {
            setProperty(transaction, VdbLexicon.DataRole.Permission.ALLOW_READ, newAllowRead);

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
     * @see org.komodo.relational.vdb.Permission#setAllowUpdate(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAllowUpdate( final UnitOfWork uow,
                                final boolean newAllowUpdate ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("permissionimpl-setAllowUpdate", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setAllowUpdate: transaction = '{0}', newAllowUpdate = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newAllowUpdate);
        }

        try {
            setProperty(transaction, VdbLexicon.DataRole.Permission.ALLOW_UPDATE, newAllowUpdate);

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

}
