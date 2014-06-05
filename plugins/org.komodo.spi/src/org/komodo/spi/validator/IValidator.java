/*************************************************************************************
 * Copyright (c) 2014 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.komodo.spi.validator;

import java.util.Collection;

import org.komodo.spi.query.metadata.IQueryMetadataInterface;
import org.komodo.spi.query.sql.ILanguageVisitor;
import org.komodo.spi.query.sql.lang.ILanguageObject;

/**
 *
 * @param <L> 
 */
public interface IValidator<L extends ILanguageObject<? extends ILanguageVisitor>> {
    
    /**
     *
     */
    public interface IValidatorReport {

        /**
         * @return
         */
        boolean hasItems();

        /**
         * @return
         */
        Collection<? extends IValidatorFailure> getItems();
        
    }

    public interface IValidatorFailure {

        enum VFStatus {
            ERROR,
            WARNING
        }

        /**
         * @return
         */
        VFStatus getStatus();
        
    }
    
    /**
     * Validate the given command
     * 
     * @param languageObject
     * @param queryMetadata
     * 
     * @return report of validation
     * @throws Exception 
     */
    IValidatorReport validate(L languageObject, IQueryMetadataInterface queryMetadata) throws Exception;

}
