/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.vdb.internal;

import org.komodo.relational.internal.RelationalChildRestrictedObject;
import org.komodo.relational.vdb.Mask;
import org.komodo.relational.vdb.Permission;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * An implementation of a VDB permission mask.
 */
public final class MaskImpl extends RelationalChildRestrictedObject implements Mask {

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
    public MaskImpl( final UnitOfWork uow,
                     final Repository repository,
                     final String workspacePath ) throws KException {
        super(uow, repository, workspacePath);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return Mask.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Mask#getOrder(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getOrder( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.STRING, "getOrder", VdbLexicon.DataRole.Permission.Mask.ORDER); //$NON-NLS-1$
    }

    /**
    * {@inheritDoc}
    *
    * @see org.komodo.relational.internal.RelationalObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
    */
    @Override
    public Permission getParent(final UnitOfWork transaction) throws KException {
    	ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
    	ArgCheck.isTrue((transaction.getState() == State.NOT_STARTED), "transaction state must be NOT_STARTED"); //$NON-NLS-1$

    	final KomodoObject grouping = super.getParent(transaction);
    	final Permission result = Permission.RESOLVER.resolve(transaction, grouping.getParent(transaction));
    	return result;
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
     * @see org.komodo.relational.vdb.Mask#setOrder(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setOrder( final UnitOfWork uow,
                          final String newOrder ) throws KException {
        setObjectProperty(uow, "setOrder", VdbLexicon.DataRole.Permission.Mask.ORDER, newOrder); //$NON-NLS-1$
    }

}
