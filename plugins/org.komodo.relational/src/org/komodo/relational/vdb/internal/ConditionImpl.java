/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.vdb.internal;

import org.komodo.relational.internal.RelationalChildRestrictedObject;
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.Permission;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * An implementation of a VDB permission condition.
 */
public final class ConditionImpl extends RelationalChildRestrictedObject implements Condition {

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
    public ConditionImpl( final UnitOfWork uow,
                          final Repository repository,
                          final String workspacePath ) throws KException {
        super(uow, repository, workspacePath);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return Condition.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoObject getParent( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject grouping = super.getParent( transaction );
        final KomodoObject result = Permission.RESOLVER.resolve( transaction, grouping.getParent( transaction ) );
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
     * @see org.komodo.relational.vdb.Condition#isConstraint(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isConstraint( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.BOOLEAN, "isConstraint", //$NON-NLS-1$
                                 VdbLexicon.DataRole.Permission.Condition.CONSTRAINT);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Condition#setConstraint(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setConstraint( final UnitOfWork uow,
                               final boolean newConstraint ) throws KException {
        setObjectProperty(uow, "setConstraint", VdbLexicon.DataRole.Permission.Condition.CONSTRAINT, newConstraint); //$NON-NLS-1$
    }

}
