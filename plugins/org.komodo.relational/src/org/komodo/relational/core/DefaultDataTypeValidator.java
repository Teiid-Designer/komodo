/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.core;

import org.komodo.relational.Messages;
import org.komodo.relational.Messages.RELATIONAL;
import org.komodo.spi.outcome.IOutcome;
import org.komodo.spi.outcome.OutcomeFactory;

/**
 *
 */
public class DefaultDataTypeValidator implements DataTypeValidator {

	/* (non-Javadoc)
	 * @see org.komodo.relational.core.DataTypeValidator#validate(org.komodo.relational.core.DataType)
	 */
	@Override
	public IOutcome validate(DataType dataType) {
    	IOutcome theOutcome = null;
    	
    	// --------------------------------------------------
    	// Name Validation - done for all Relational Objects
    	// --------------------------------------------------
    	String dTypeName = dataType.getName();
		if( dTypeName == null || dTypeName.length() == 0 ) {
		    theOutcome = OutcomeFactory.getInstance().createError(
					  Messages.getString(RELATIONAL.validate_error_nameCannotBeNullOrEmpty, dTypeName) );
			return theOutcome;
		}
		
		// Validate name is a valid datatype
    	String[] allowedValues = DataTypes.AS_ARRAY;
    	boolean matchFound = false;
    	for(int i=0; i<allowedValues.length; i++) {
    		if(allowedValues[i].equalsIgnoreCase(dTypeName)) {
    			matchFound = true;
    		}
    	}
    	if(!matchFound) {
		    theOutcome = OutcomeFactory.getInstance().createError(
					  Messages.getString(RELATIONAL.dataTypeError_NameNotValid, dTypeName) );
			return theOutcome;
    	}
				
		return OutcomeFactory.getInstance().createOK();
	}

}
