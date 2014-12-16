/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import org.komodo.relational.model.Index;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.Constraint;

/**
 * An implementation of a relational model index.
 */
public final class IndexImpl extends TableConstraintImpl implements Index {

    /**
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public IndexImpl( final Repository repository,
                      final String workspacePath ) throws KException {
        super(repository, workspacePath);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.TableConstraint#getConstraintType()
     */
    @Override
    public ConstraintType getConstraintType() {
        return ConstraintType.INDEX;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Index#getExpression(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getExpression( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("indeximpl-getExpression", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            String result = null;
            final Property property = getProperty(transaction, Constraint.EXPRESSION);

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
     * @see org.komodo.relational.model.Index#setExpression(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setExpression( final UnitOfWork uow,
                               final String newExpression ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("indeximpl-setExpression", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setExpression: transaction = '{0}', newExpression = '{1}'", //$NON-NLS-1$
                         transaction.getName(),
                         newExpression);
        }

        try {
            setProperty(transaction, Constraint.EXPRESSION, StringUtils.isBlank(newExpression) ? null : newExpression);

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

}
