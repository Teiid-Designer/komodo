/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.dataservice.internal;

import java.util.Properties;

import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.dataservice.VdbEntry;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.internal.VdbImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * An implementation of a VDB entry in a data service.
 */
public class VdbEntryImpl extends RelationalObjectImpl implements VdbEntry {

    private static final String ARCHIVE_FOLDER = "vdbs/"; //$NON-NLS-1$

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the object is located (cannot be <code>null</code>)
     * @param path
     *        the workspace path (cannot be <code>null</code> or empty)
     * @throws KException
     *         if an error occurs
     */
    public VdbEntryImpl( final UnitOfWork uow,
                         final Repository repository,
                         final String path ) throws KException {
        super( uow, repository, path );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Exportable#export(org.komodo.spi.repository.Repository.UnitOfWork, java.util.Properties)
     */
    @Override
    public byte[] export( final UnitOfWork uow,
                          final Properties properties ) throws KException {
        final Vdb vdb = getReference( uow );

        if ( vdb == null ) {
            throw new KException( Messages.getString( Relational.EXPORT_FAILED_NO_CONTENT, getAbsolutePath() ) );
        }

        return vdb.export( uow, properties );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.DataServiceEntry#getArchiveFolder()
     */
    @Override
    public String getArchiveFolder() {
        return ARCHIVE_FOLDER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.DataServiceEntry#getReference(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Vdb getReference( final UnitOfWork uow ) throws KException {
        if ( hasProperty( uow, DataVirtLexicon.VdbEntry.VDB_REF ) ) {
            final String refId = getProperty( uow, DataVirtLexicon.VdbEntry.VDB_REF ).getStringValue( uow );
            final KomodoObject kobj = getRepository().getUsingId( uow, refId );

            if ( kobj == null ) {
                throw new KException( Messages.getString( Messages.Relational.REFERENCED_RESOURCE_NOT_FOUND,
                                                          VdbLexicon.Vdb.VIRTUAL_DATABASE,
                                                          refId ) );
            }

            return new VdbImpl( uow, getRepository(), kobj.getAbsolutePath() );
        }

        return null;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.VdbEntry#getVdbName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getVdbName( final UnitOfWork transaction ) throws KException {
        return getObjectProperty( transaction,
                                  PropertyValueType.STRING,
                                  "getVdbName", //$NON-NLS-1$
                                  DataVirtLexicon.VdbEntry.VDB_NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.VdbEntry#getVdbVersion(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getVdbVersion( final UnitOfWork transaction ) throws KException {
        return getObjectProperty( transaction,
                                  PropertyValueType.STRING,
                                  "getVdbVersion", //$NON-NLS-1$
                                  DataVirtLexicon.VdbEntry.VDB_VERSION );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.VdbEntry#setVdbName(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setVdbName( final UnitOfWork transaction,
                            final String vdbName ) throws KException {
        setObjectProperty( transaction, "setVdbName", DataVirtLexicon.VdbEntry.VDB_NAME, vdbName ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.VdbEntry#setVdbVersion(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setVdbVersion( final UnitOfWork transaction,
                               final String vdbVersion ) throws KException {
        setObjectProperty( transaction, "setVdbVersion", DataVirtLexicon.VdbEntry.VDB_VERSION, vdbVersion ); //$NON-NLS-1$
    }

}
