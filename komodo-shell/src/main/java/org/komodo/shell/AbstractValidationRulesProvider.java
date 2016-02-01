/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.shell;

import org.komodo.shell.api.ValidationRulesProvider;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * Abstract Validation Rules Provider
 */
public abstract class AbstractValidationRulesProvider implements ValidationRulesProvider {

    /* (non-Javadoc)
     * @see org.komodo.shell.api.ValidationRulesProvider#importRules(org.komodo.spi.repository.Repository, org.komodo.spi.repository.Repository.UnitOfWork, java.lang.boolean)
     */
    @Override
    public abstract void importRules( Repository repo, UnitOfWork uow, boolean overwriteExisting ) throws KException;

}
