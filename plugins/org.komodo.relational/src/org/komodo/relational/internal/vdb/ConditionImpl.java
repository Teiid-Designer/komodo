/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.internal.vdb;

import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.vdb.Condition;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * An implementation of a VDB permission condition.
 */
public final class ConditionImpl extends RelationalObjectImpl implements Condition {

    /**
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public ConditionImpl( final Repository repository,
                          final String workspacePath ) throws KException {
        super(repository, workspacePath);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Condition#isConstraint(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isConstraint( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow,
                                 Property.ValueType.BOOLEAN,
                                 "isConstraint",  //$NON-NLS-1$
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
