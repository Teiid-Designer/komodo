/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.vdb.internal;

import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.internal.RelationalChildRestrictedObject;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.repository.ObjectImpl;
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
 * An implementation of a referenced VDB.
 */
public class VdbImportImpl extends RelationalChildRestrictedObject implements VdbImport {

    /**
     * The resolver of a {@link VdbImport}.
     */
    public static final TypeResolver< VdbImport > RESOLVER = new TypeResolver< VdbImport >() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public VdbImport create( final UnitOfWork transaction,
                                 final Repository repository,
                                 final KomodoObject parent,
                                 final String id,
                                 final RelationalProperties properties ) throws KException {
            final AdapterFactory adapter = new AdapterFactory( repository );
            final Vdb parentVdb = adapter.adapt( transaction, parent, Vdb.class );

            if ( parentVdb == null ) {
                throw new KException( Messages.getString( Relational.INVALID_PARENT_TYPE,
                                                          parent.getAbsolutePath(),
                                                          VdbImport.class.getSimpleName() ) );
            }

            return parentVdb.addImport( transaction, id );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#identifier()
         */
        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#owningClass()
         */
        @Override
        public Class< VdbImportImpl > owningClass() {
            return VdbImportImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final KomodoObject kobject ) throws KException {
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, VdbLexicon.ImportVdb.IMPORT_VDB );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public VdbImport resolve( final UnitOfWork transaction,
                                  final KomodoObject kobject ) throws KException {
            return new VdbImportImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

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
    public VdbImportImpl( final UnitOfWork uow,
                          final Repository repository,
                          final String workspacePath ) throws KException {
        super(uow, repository, workspacePath);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return RESOLVER.identifier();
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
        final KomodoObject result = resolveType( transaction, grouping.getParent( transaction ) );
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
     * @see org.komodo.relational.vdb.VdbImport#getVersion(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public int getVersion( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.INTEGER, "getVersion", VdbLexicon.ImportVdb.VERSION); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.VdbImport#isImportDataPolicies(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isImportDataPolicies( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.BOOLEAN, "isImportDataPolicies", //$NON-NLS-1$
                                 VdbLexicon.ImportVdb.IMPORT_DATA_POLICIES);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.VdbImport#setImportDataPolicies(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setImportDataPolicies( final UnitOfWork uow,
                                       final boolean newImportDataPolicies ) throws KException {
        setObjectProperty(uow, "setImportDataPolicies", VdbLexicon.ImportVdb.IMPORT_DATA_POLICIES, newImportDataPolicies); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.VdbImport#setVersion(org.komodo.spi.repository.Repository.UnitOfWork, int)
     */
    @Override
    public void setVersion( final UnitOfWork uow,
                            final int newVersion ) throws KException {
        setObjectProperty(uow, "setVersion", VdbLexicon.ImportVdb.VERSION, newVersion); //$NON-NLS-1$
    }

}
