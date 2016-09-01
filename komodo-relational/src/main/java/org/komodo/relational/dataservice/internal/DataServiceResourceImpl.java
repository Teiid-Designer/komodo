/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.dataservice.internal;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.dataservice.DataServiceResource;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.FileUtils;

/**
 * A base implementation of a data service resource.
 *
 * @param <T>
 *        the type of resource
 */
public abstract class DataServiceResourceImpl< T extends DataServiceResource > extends RelationalObjectImpl
    implements DataServiceResource {

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the object is located (cannot be <code>null</code>)
     * @param path
     *        the workspace path (cannot be <code>null</code> or empty)
     * @throws KException
     *         if an error occurs
     */
    protected DataServiceResourceImpl( final UnitOfWork transaction,
                                       final Repository repository,
                                       final String path ) throws KException {
        super( transaction, repository, path );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Exportable#export(org.komodo.spi.repository.Repository.UnitOfWork, java.util.Properties)
     */
    @Override
    public byte[] export( final UnitOfWork transaction,
                          final Properties properties ) throws KException {
        InputStream stream = getContent( transaction );

        if ( stream == null ) {
            throw new KException( Messages.getString( Relational.EXPORT_FAILED_NO_CONTENT, getAbsolutePath() ) );
        }

        byte[] contents = null;

        try {
            contents = FileUtils.write( stream );
        } catch ( IOException e ) {
            handleError( e );
        }

        return contents;
    }

}
