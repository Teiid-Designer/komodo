/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.internal;

import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A base implementation of a relational object that does not permit children.
 */
public abstract class RelationalChildRestrictedObject extends RelationalObjectImpl {

    protected RelationalChildRestrictedObject( final UnitOfWork uow,
                                               final Repository repository,
                                               final String path ) throws KException {
        super( uow, repository, path );
    }

    protected RelationalChildRestrictedObject( final UnitOfWork uow,
                                               final Repository repository,
                                               final String path,
                                               final int index ) throws KException {
        super( uow, repository, path, index );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#addChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.String)
     * @throws UnsupportedOperationException
     *         if this method is called
     */
    @Override
    public final KomodoObject addChild( final UnitOfWork uow,
                                        final String name,
                                        final String primaryType ) {
        throw new UnsupportedOperationException( "Children cannot be added to objects of type " + getClass().getName() ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @return an empty array
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public final KomodoObject[] getChildren( final UnitOfWork uow ) {
        return KomodoObject.EMPTY_ARRAY;
    }

    /**
     * {@inheritDoc}
     *
     * @return an empty array
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildren(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public final KomodoObject[] getChildren( final UnitOfWork uow,
                                             final String name ) {
        return KomodoObject.EMPTY_ARRAY;
    }

    /**
     * {@inheritDoc}
     *
     * @return an empty array
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildrenOfType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public final KomodoObject[] getChildrenOfType( final UnitOfWork uow,
                                                   final String type ) {
        return KomodoObject.EMPTY_ARRAY;
    }

    /**
     * {@inheritDoc}
     *
     * @return <code>false</code>
     * @see org.komodo.repository.ObjectImpl#hasChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public final boolean hasChild( final UnitOfWork uow,
                                   final String name ) {
        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @return <code>false</code>
     * @see org.komodo.repository.ObjectImpl#hasChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public final boolean hasChildren( final UnitOfWork uow ) {
        return false;
    }
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#isChildRestricted()
     */
    @Override
    public final boolean isChildRestricted() {
        return true;
    }

}
