/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.model.legacy;

import org.komodo.spi.outcome.Outcome;
import org.komodo.utils.StringNameValidator;

/**
 * RelationalValidator interface
 */
public interface RelationalValidator {

	/**
     * Validate the Relational Object
     * @param relationalObj the object
     * @return the validation outcome
     */
    public Outcome validate(RelationalObject relationalObj);

    /**
     * Set the name validator
     * @param nameValidator the name validator
     */
    public void setNameValidator(StringNameValidator nameValidator);

    /**
     * Set the DataType validator
     * @param datatypeValidator the DataType validator
     */
    public void setDataTypeValidator(DataTypeValidator datatypeValidator);
    
}
