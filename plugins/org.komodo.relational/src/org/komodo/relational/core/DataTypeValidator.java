/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.core;

import org.komodo.spi.outcome.IOutcome;

/**
 *
 */
public interface DataTypeValidator {

	/**
     * Validate the DataType
     * @param dataType the DataType
     * @return the validation outcome
     */
    public IOutcome validate(DataType dataType);

}
