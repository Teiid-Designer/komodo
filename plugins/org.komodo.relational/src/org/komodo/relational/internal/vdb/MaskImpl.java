/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.internal.vdb;

import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.vdb.Mask;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * An implementation of a VDB permission mask.
 */
public final class MaskImpl extends RelationalObjectImpl implements Mask {

    /**
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public MaskImpl( final Repository repository,
                     final String workspacePath ) throws KException {
        super(repository, workspacePath);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Mask#getOrder(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getOrder( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("maskimpl-getOrder", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            String result = null;
            final Property property = getProperty(transaction, VdbLexicon.DataRole.Permission.Mask.ORDER);

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
     * @see org.komodo.relational.vdb.Mask#setOrder(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setOrder( final UnitOfWork uow,
                          final String newOrder ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("maskimpl-setOrder", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setOrder: transaction = '{0}', newOrder = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newOrder);
        }

        try {
            setProperty(transaction,
                        VdbLexicon.DataRole.Permission.Mask.ORDER,
                        StringUtils.isBlank(newOrder) ? null : newOrder);

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

}
