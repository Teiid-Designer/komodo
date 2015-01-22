/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.internal.model;

import org.komodo.relational.model.PrimaryKey;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository;

/**
 * An implementation of a relational model primary key.
 */
public final class PrimaryKeyImpl extends TableConstraintImpl implements PrimaryKey {

    /**
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public PrimaryKeyImpl( final Repository repository,
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
        return CONSTRAINT_TYPE;
    }

}
