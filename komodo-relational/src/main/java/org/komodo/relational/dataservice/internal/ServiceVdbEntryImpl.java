/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.dataservice.internal;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.dataservice.ServiceVdbEntry;
import org.komodo.relational.dataservice.VdbEntry;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

/**
 * An implementation of a Service VDB entry in a data service.
 */
public class ServiceVdbEntryImpl extends VdbEntryImpl implements ServiceVdbEntry {

    /**
     * The allowed child types.
     */
    private static final KomodoType[] CHILD_TYPES = new KomodoType[] { VdbEntry.IDENTIFIER };

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
    public ServiceVdbEntryImpl( final UnitOfWork uow,
                                final Repository repository,
                                final String path ) throws KException {
        super( uow, repository, path );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.ServiceVdbEntry#addDependency(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.vdb.Vdb)
     */
    @Override
    public VdbEntry addDependency( final UnitOfWork uow,
                                   final Vdb dependency ) throws KException {
        final VdbEntry entry = RelationalModelFactory.createVdbEntry( uow,
                                                                      getRepository(),
                                                                      this,
                                                                      Objects.requireNonNull( dependency, "dependency" ) //$NON-NLS-1$
                                                                             .getName( uow ) );
        entry.setVdbName( uow, dependency.getName( uow ) );
        entry.setVdbVersion( uow, Integer.toString( dependency.getVersion( uow ) ) );
        entry.setReference( uow, dependency );
        return entry;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.ServiceVdbEntry#addDependencyEntry(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public VdbEntry addDependencyEntry( final UnitOfWork uow,
                                        final String dependencyEntryName ) throws KException {
        return RelationalModelFactory.createVdbEntry( uow, getRepository(), this, dependencyEntryName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.internal.VdbEntryImpl#getArchiveFolder()
     */
    @Override
    public String getArchiveFolder() {
        return StringConstants.EMPTY_STRING;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildTypes()
     */
    @Override
    public KomodoType[] getChildTypes() {
        return CHILD_TYPES;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.ServiceVdbEntry#getDependencies(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public VdbEntry[] getDependencies( final UnitOfWork uow ) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< VdbEntry > result = new ArrayList<>();

        for ( final KomodoObject kobject : super.getChildrenOfType( uow, DataVirtLexicon.VdbEntry.NODE_TYPE ) ) {
            final VdbEntry entry = new VdbEntryImpl( uow, getRepository(), kobject.getAbsolutePath() );
            result.add( entry );
        }

        if ( result.isEmpty() ) {
            return VdbEntry.NO_ENTRIES;
        }

        return result.toArray( new VdbEntry[ result.size() ] );
    }

}
