/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import org.komodo.relational.RelationalProperties;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.model.Table;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;

/**
 * An implementation of a relational model DDL statement option.
 */
public final class StatementOptionImpl extends RelationalObjectImpl implements StatementOption {

    /**
     * The resolver of a {@link StatementOption}.
     */
    public static final TypeResolver RESOLVER = new TypeResolver() {

        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }

        @Override
        public Class<? extends KomodoObject> owningClass() {
            return StatementOptionImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final Repository repository,
                                   final KomodoObject kobject ) {
            try {
                ObjectImpl.validateType(transaction, repository, kobject, StandardDdlLexicon.TYPE_STATEMENT_OPTION);
                return true;
            } catch (final Exception e) {
                // not resolvable
            }

            return false;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public StatementOption resolve( final UnitOfWork transaction,
                                        final Repository repository,
                                        final KomodoObject kobject ) throws KException {
            return new StatementOptionImpl(transaction, repository, kobject.getAbsolutePath());
        }

        @Override
        public StatementOption create(UnitOfWork transaction,
                                                      KomodoObject parent,
                                                      String id,
                                                      RelationalProperties properties) throws KException {
            AdapterFactory adapter = new AdapterFactory(parent.getRepository());
            Object optionValueValue = properties.getValue(StandardDdlLexicon.VALUE);
            String optionValue = optionValueValue == null ? null : optionValueValue.toString();
            Table parentTable = adapter.adapt(transaction, parent, Table.class);
            return RelationalModelFactory.createStatementOption(transaction, parent.getRepository(), parentTable, id, optionValue);
        }

    };

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs or if node at specified path is not a statement option
     */
    public StatementOptionImpl( final UnitOfWork uow,
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
     * @see org.komodo.relational.model.StatementOption#getOption(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getOption( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, Property.ValueType.STRING, "getOption", StandardDdlLexicon.VALUE); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.StatementOption#setOption(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setOption( final UnitOfWork uow,
                           final String newOption ) throws KException {
        ArgCheck.isNotEmpty(newOption, "newOption"); //$NON-NLS-1$
        setObjectProperty(uow, "setOption", StandardDdlLexicon.VALUE, newOption); //$NON-NLS-1$
    }

}
