/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.dataservice.internal;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.dataservice.DataServiceResource;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.FileUtils;
import org.modeshape.jcr.JcrLexicon;
import org.modeshape.jcr.api.JcrConstants;

/**
 * A base implementation of a data service resource.
 *
 * @param <T>
 *        the type of resource
 */
public abstract class DataServiceResourceImpl< T extends DataServiceResource >
    extends RelationalObjectImpl
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

    @Override
    public InputStream getContent( final UnitOfWork transaction ) throws KException {
        if ( !hasChild( transaction, JcrLexicon.CONTENT.getString() ) ) return null;

        final KomodoObject fileNode = getChild( transaction, JcrConstants.JCR_CONTENT, JcrConstants.NT_RESOURCE );
        final Property property = fileNode.getProperty( transaction, JcrConstants.JCR_DATA );
        return property.getBinaryValue( transaction );
    }

    @Override
    public void setContent( final UnitOfWork transaction,
                             final byte[] content ) throws KException {
        KomodoObject fileNode = null;

        if ( !hasChild( transaction, JcrConstants.JCR_CONTENT ) ) {
            fileNode = addChild( transaction, JcrConstants.JCR_CONTENT, JcrConstants.NT_RESOURCE );
        } else {
            fileNode = getChild( transaction, JcrConstants.JCR_CONTENT );
        }

        final ByteArrayInputStream stream = new ByteArrayInputStream( content );
        fileNode.setProperty( transaction, JcrConstants.JCR_DATA, stream );
    }
}
