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

import java.util.List;

import org.komodo.spi.query.sql.lang.ICommand;
import org.komodo.spi.query.sql.symbol.IElementSymbol;
import org.komodo.spi.validator.IValidator.IValidatorReport;

/**
 *
 */
public interface IUpdateValidator<C extends ICommand, ES extends IElementSymbol> {

    /**
     * Update type enumerator
     */
    public enum TransformUpdateType {
        /**
         * The default handling should be used
         */
        INHERENT, 
        /**
         * An instead of trigger (TriggerAction) has been defined
         */
        INSTEAD_OF
    }
    
    /**
     * Validate the command
     * 
     * @param command
     * @param elemSymbols
     * @throws Exception 
     */
    void validate(C command, List<ES> elemSymbols) throws Exception;

    /**
     * @return insert report
     */
    IValidatorReport getInsertReport();

    /**
     * @return update report
     */
    IValidatorReport getUpdateReport();

    /**
     * @return delete report
     */
    IValidatorReport getDeleteReport();

    /**
     * @return report
     */
    IValidatorReport getReport();
}
