/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.StatementOption;
import org.komodo.spi.KException;
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

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#validateInitialState(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    protected void validateInitialState( final UnitOfWork uow,
                                         final String path ) throws KException {
        validateType(uow, path, StandardDdlLexicon.TYPE_STATEMENT_OPTION);
    }

}
