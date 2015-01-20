/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import org.komodo.relational.model.RelationalObject;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository;
import org.komodo.utils.KLog;

/**
 * A base implementation of a relational object.
 */
public abstract class RelationalObjectImpl extends ObjectImpl implements RelationalObject {

    protected static final KLog LOGGER = KLog.getLogger();

    protected RelationalObjectImpl( final Repository repository,
                                    final String path ) throws KException {
        this(repository, path, 0);
    }

    protected RelationalObjectImpl( final Repository repository,
                                    final String path,
                                    final int index ) throws KException {
        super(repository, path, index);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#toString()
     */
    @Override
    public String toString() {
        return getAbsolutePath();
    }

}
