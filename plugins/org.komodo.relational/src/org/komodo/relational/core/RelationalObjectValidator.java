/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.core;

import java.util.List;

import org.komodo.relational.Messages;
import org.komodo.relational.Messages.RELATIONAL;
import org.komodo.relational.constants.RelationalConstants.DIRECTION;
import org.komodo.relational.constants.RelationalConstants.TYPES;
import org.komodo.relational.model.AccessPattern;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Index;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.ProcedureResultSet;
import org.komodo.relational.model.RelationalObject;
import org.komodo.relational.model.Schema;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.relational.model.View;
import org.komodo.spi.outcome.IOutcome;
import org.komodo.spi.outcome.IOutcome.Level;
import org.komodo.spi.outcome.OutcomeFactory;
import org.komodo.utils.StringNameValidator;

/**
 * RelationalObjectValidator.  This validation returns the first ERROR that is found for the RelationalObject.  If no
 * errors are found, the first warning is returned.
 */
public class RelationalObjectValidator implements RelationalValidator {

	StringNameValidator nameValidator = new RelationalStringNameValidator(false);
	
	/* (non-Javadoc)
	 * @see org.komodo.relational.core.RelationalValidator#setNameValidator(org.komodo.utils.StringNameValidator)
	 */
	@Override
	public void setNameValidator(StringNameValidator nameValidator) {
		this.nameValidator = nameValidator;
	}
	
	private StringNameValidator getNameValidator() {
		return this.nameValidator;
	}

	/* (non-Javadoc)
	 * @see org.komodo.relational.core.RelationalValidator#validate(org.komodo.relational.model.RelationalObject)
	 */
    @Override
	public IOutcome validate(RelationalObject relationalObj) {
    	IOutcome theOutcome = null;
    	
    	// --------------------------------------------------
    	// Name Validation - done for all Relational Objects
    	// --------------------------------------------------
		if( relationalObj.getName() == null || relationalObj.getName().length() == 0 ) {
		    theOutcome = OutcomeFactory.getInstance().createError(
					  Messages.getString(RELATIONAL.validate_error_nameCannotBeNullOrEmpty, relationalObj.getDisplayName()) );
			return theOutcome;
		}
		// Validate non-null string
		String errorMessage = getNameValidator().checkValidName(relationalObj.getName());
		if( errorMessage != null && !errorMessage.isEmpty() ) {
			theOutcome = OutcomeFactory.getInstance().createError(errorMessage);
			return theOutcome;
		}
		
    	// --------------------------------------------------
    	// Object-specific validation rules
    	// --------------------------------------------------
		int objType = relationalObj.getType();
		switch(objType) {
		case TYPES.TABLE: 
			return validateTable((Table)relationalObj);
		case TYPES.COLUMN:
			return validateColumn((Column)relationalObj);
		case TYPES.PROCEDURE:
			return validateProcedure((Procedure)relationalObj);
		case TYPES.PARAMETER:
			return validateParameter((Parameter)relationalObj);
		case TYPES.RESULT_SET:
			return validateProcedureResultSet((ProcedureResultSet)relationalObj);
		case TYPES.SCHEMA:
			return validateSchema((Schema)relationalObj);
		case TYPES.VIEW:
			return validateView((View)relationalObj);
		case TYPES.UC:
			return validateUniqueConstraint((UniqueConstraint)relationalObj);
		case TYPES.AP:
			return validateAccessPattern((AccessPattern)relationalObj);
		case TYPES.PK:
			return validatePrimaryKey((PrimaryKey)relationalObj);
		case TYPES.FK:
			return validateForeignKey((ForeignKey)relationalObj);
		case TYPES.INDEX:
			return validateIndex((Index)relationalObj);
		case TYPES.MODEL:
			return validateModel((Model)relationalObj);
		}
		
		return OutcomeFactory.getInstance().createOK();
    }
    
    /**
     * Table validation
     * @param relationalTable the table
     * @return the validation status
     */
	private IOutcome validateTable(Table relationalTable) {
    	IOutcome theOutcome = null;
    	
		if( relationalTable.isMaterialized() && relationalTable.getMaterializedTable() == null ) {
			theOutcome = OutcomeFactory.getInstance().createError(
					Messages.getString(RELATIONAL.validate_error_materializedTableHasNoTableDefined) );
			return theOutcome;
		}
		
		if( relationalTable.getPrimaryKey() != null) {
			theOutcome = validate(relationalTable.getPrimaryKey());
			if(theOutcome.getLevel() == Level.ERROR) return theOutcome;
		}
		
		if( relationalTable.getUniqueContraint() != null ) {
			theOutcome = validate(relationalTable.getUniqueContraint());
			if(theOutcome.getLevel() == Level.ERROR) return theOutcome;
		}
		
		for( ForeignKey fk : relationalTable.getForeignKeys() ) {
			theOutcome = validate(fk);
			if(theOutcome.getLevel() == Level.ERROR) return theOutcome;
		}
		
		// Check Column Status values
		for( Column col : relationalTable.getColumns() ) {
			theOutcome = validate(col);
			if( theOutcome.getLevel() == Level.ERROR ) {
				return theOutcome;
			}
		}
		
		// Check Column Status values
		for( Column outerColumn : relationalTable.getColumns() ) {
			for( Column innerColumn : relationalTable.getColumns() ) {
				if( outerColumn != innerColumn ) {
					if( outerColumn.getName().equalsIgnoreCase(innerColumn.getName())) {
						theOutcome = OutcomeFactory.getInstance().createError(
								Messages.getString(RELATIONAL.validate_error_duplicateColumnNamesInTable, relationalTable.getName()) );
						return theOutcome;
					}
				}
			}
		}
		
		if( relationalTable.getColumns().isEmpty() ) {
			if( relationalTable.getParent() != null && relationalTable.getParent() instanceof Procedure ) {
				theOutcome = OutcomeFactory.getInstance().createWarning(
						Messages.getString(RELATIONAL.validate_warning_noColumnsDefinedForResultSet) ); 
				return theOutcome;
			} else {
				theOutcome = OutcomeFactory.getInstance().createWarning(
						Messages.getString(RELATIONAL.validate_warning_noColumnsDefined) ); 
				return theOutcome;
			}
		}
		
		return OutcomeFactory.getInstance().createOK();
	}
	
    /**
     * Column validation
     * @param relationalColumn the column
     * @return the validation status
     */
	private IOutcome validateColumn(Column relationalColumn) {
		return OutcomeFactory.getInstance().createOK();
	}
	
    /**
     * AccessPattern validation
     * @param relationalAP the access pattern
     * @return the validation status
     */
	private IOutcome validateAccessPattern(AccessPattern relationalAP) {
		return OutcomeFactory.getInstance().createOK();
	}
	
    /**
     * ForeignKey validation
     * @param relationalFK the foreign key
     * @return the validation status
     */
	private IOutcome validateForeignKey(ForeignKey relationalFK) {
    	IOutcome theOutcome = null;
		
		if( relationalFK.getColumns().isEmpty() ) {
			theOutcome = OutcomeFactory.getInstance().createError(
					Messages.getString(RELATIONAL.validate_error_fkNoColumnsDefined, relationalFK.getName()) );
			return theOutcome;
		}
				
		if( relationalFK.getUniqueKeyName() == null || relationalFK.getUniqueKeyName().length() == 0 ) {
			theOutcome = OutcomeFactory.getInstance().createError(
					Messages.getString(RELATIONAL.validate_error_fKUniqueKeyNameIsUndefined, relationalFK.getName()) );
			return theOutcome;
		}
		
		if( relationalFK.getUniqueKeyTableName() == null || relationalFK.getUniqueKeyTableName().length() == 0 ) {
			theOutcome = OutcomeFactory.getInstance().createError(
					Messages.getString(RELATIONAL.validate_error_fKReferencedUniqueKeyTableIsUndefined) );
			return theOutcome;
		}
    	
		return OutcomeFactory.getInstance().createOK();
	}
	
    /**
     * PrimaryKey validation
     * @param relationalPK the Primary Key
     * @return the validation status
     */
	private IOutcome validatePrimaryKey(PrimaryKey relationalPK) {
    	IOutcome theOutcome = null;
    	
		if( relationalPK.getColumns().isEmpty() ) {
			theOutcome = OutcomeFactory.getInstance().createError(
					Messages.getString(RELATIONAL.validate_error_pkNoColumnsDefined, relationalPK.getName()) );
			return theOutcome;
		}

		return OutcomeFactory.getInstance().createOK();
	}
	
    /**
     * Index validation
     * @param relationalIndex the Index
     * @return the validation status
     */
	private IOutcome validateIndex(Index relationalIndex) {
    	IOutcome theOutcome = null;
    	
		// Check Column Status values
		for( Column col : relationalIndex.getColumns() ) {
			if( col.getOutcome().getLevel() == Level.ERROR ) {
				theOutcome = OutcomeFactory.getInstance().createError(col.getOutcome().getMessage() );
				return theOutcome;
			}
		}
		
		// Check Column Status values
		for( Column outerColumn : relationalIndex.getColumns() ) {
			for( Column innerColumn : relationalIndex.getColumns() ) {
				if( outerColumn != innerColumn ) {
					if( outerColumn.getName().equalsIgnoreCase(innerColumn.getName())) {
						theOutcome = OutcomeFactory.getInstance().createError(
								Messages.getString(RELATIONAL.validate_error_duplicateColumnNamesReferencedInIndex, relationalIndex.getName()) ); 
						return theOutcome;
					}
				}
			}
		}
		
		if( relationalIndex.getColumns().isEmpty() ) {
			theOutcome = OutcomeFactory.getInstance().createError(
					Messages.getString(RELATIONAL.validate_warning_noColumnReferencesDefined, relationalIndex.getName()) );
			return theOutcome;
		}
    	
		return OutcomeFactory.getInstance().createOK();
	}
	
    /**
     * Model validation
     * @param relationalModel the model
     * @return the validation status
     */
	private IOutcome validateModel(Model relationalModel) {
		return OutcomeFactory.getInstance().createOK();
	}
	
    /**
     * Parameter validation
     * @param relationalParmeter the Parameter
     * @return the validation status
     */
	private IOutcome validateParameter(Parameter relationalParmeter) {
    	IOutcome theOutcome = null;
    	
		// Parameter directions check
		Procedure parentProcedure = (Procedure)relationalParmeter.getParent();
		if(parentProcedure!=null && parentProcedure.isFunction()) {
			if( ! relationalParmeter.getDirection().equalsIgnoreCase(DIRECTION.IN) &&
					! relationalParmeter.getDirection().equalsIgnoreCase(DIRECTION.RETURN)	) {
				theOutcome = OutcomeFactory.getInstance().createError(
						Messages.getString(RELATIONAL.validate_error_invalidParameterDirectionInFunction) ); 
				return theOutcome;
			}
		}
    	
		return OutcomeFactory.getInstance().createOK();
	}
	
    /**
     * Procedure validation
     * @param relationalProcedure the Procedure
     * @return the validation status
     */
	private IOutcome validateProcedure(Procedure relationalProcedure) {
    	IOutcome theOutcome = null;
    	
    	List<Parameter> params = relationalProcedure.getParameters();
    	
    	if(!params.isEmpty()) {
    		// Validate Parameters
    		for( Parameter param : params ) {
    			theOutcome = validate(param);
    			if(theOutcome.getLevel() == Level.ERROR) return theOutcome;
    		}

    		// Check Parameter Status values
    		for( Parameter outerParam : params ) {
    			for( Parameter innerParam : params ) {
    				if( outerParam != innerParam ) {
    					if( outerParam.getName().equalsIgnoreCase(innerParam.getName())) {
    						theOutcome = OutcomeFactory.getInstance().createError(
    								Messages.getString(RELATIONAL.validate_error_duplicateParameterNamesInProcedure, relationalProcedure.getName()) ); 
    						return theOutcome;
    					}
    				}
    			}
    		}
    	}
				
		// Check for more than one RETURN parameter if Function
		if( relationalProcedure.isFunction() ) {
			boolean foundResultParam = false;
			for( Parameter param : params ) {
				if( param.getDirection().equalsIgnoreCase(DIRECTION.RETURN)) {
					if( foundResultParam ) {
						theOutcome = OutcomeFactory.getInstance().createError(
								Messages.getString(RELATIONAL.validate_error_tooManyResultParametersInFunction) ); 
						return theOutcome;
					} else {
						foundResultParam = true;
					}
				}
			}
			
			if( relationalProcedure.isSourceFunction() ) {
				if( relationalProcedure.getResultSet() != null ) {
					theOutcome = OutcomeFactory.getInstance().createError(
							Messages.getString(RELATIONAL.validate_noResultSetAllowedInFunction) ); 
					return theOutcome;
				}
			} else {
				// Check for null category, class or method name
				if( relationalProcedure.getFunctionCategory() == null || relationalProcedure.getFunctionCategory().trim().length() == 0 ) {
					theOutcome = OutcomeFactory.getInstance().createError(
							Messages.getString(RELATIONAL.validate_categoryUndefinedForUDF) ); 
					return theOutcome;
				}
				if( relationalProcedure.getJavaClassName() == null || relationalProcedure.getJavaClassName().trim().length() == 0 ) {
					theOutcome = OutcomeFactory.getInstance().createError(
							Messages.getString(RELATIONAL.validate_javaClassUndefinedForUDF) ); 
					return theOutcome;
				}
				if( relationalProcedure.getJavaMethodName() == null || relationalProcedure.getJavaMethodName().trim().length() == 0 ) {
					theOutcome = OutcomeFactory.getInstance().createError(
							Messages.getString(RELATIONAL.validate_javaMethodUndefinedForUDF) ); 
					return theOutcome;
				}
			}
		} else {
			if( relationalProcedure.getResultSet() != null ) {
				if( relationalProcedure.getResultSet().getOutcome().getLevel() == Level.ERROR ) {
					theOutcome = OutcomeFactory.getInstance().createError(relationalProcedure.getResultSet().getOutcome().getMessage() );
					return theOutcome;
				}
				
				if( relationalProcedure.getResultSet().getOutcome().getLevel() == Level.WARNING ) {
					theOutcome = OutcomeFactory.getInstance().createError(relationalProcedure.getResultSet().getOutcome().getMessage() );
					return theOutcome;
				}
			}
		}
		
		if( params.isEmpty() ) {
			theOutcome = OutcomeFactory.getInstance().createWarning( 
					Messages.getString(RELATIONAL.validate_warning_noParametersDefined) ); 
			return theOutcome;
		}
		
		return OutcomeFactory.getInstance().createOK();
	}
	
    /**
     * ProcedureResultSet validation
     * @param relationalProcedureResultSet the ProcedureResultSet
     * @return the validation status
     */
	private IOutcome validateProcedureResultSet(ProcedureResultSet relationalProcedureResultSet) {
    	IOutcome theOutcome = null;
    	
		// Check Column Status values
		for( Column col : relationalProcedureResultSet.getColumns() ) {
			theOutcome = validate(col);
			if( theOutcome.getLevel() == Level.ERROR ) {
				return theOutcome;
			}
		}
		
		// Check Column Status values
		for( Column outerColumn : relationalProcedureResultSet.getColumns() ) {
			for( Column innerColumn : relationalProcedureResultSet.getColumns() ) {
				if( outerColumn != innerColumn ) {
					if( outerColumn.getName().equalsIgnoreCase(innerColumn.getName())) {
						theOutcome = OutcomeFactory.getInstance().createError(
								Messages.getString(RELATIONAL.validate_error_duplicateColumnNamesInTable, relationalProcedureResultSet.getName()) );
						return theOutcome;
					}
				}
			}
		}
		
		if( relationalProcedureResultSet.getColumns().isEmpty() ) {
			if( relationalProcedureResultSet.getParent() != null && relationalProcedureResultSet.getParent() instanceof Procedure ) {
				theOutcome = OutcomeFactory.getInstance().createWarning(
						Messages.getString(RELATIONAL.validate_warning_noColumnsDefinedForResultSet) ); 
				return theOutcome;
			} else {
				theOutcome = OutcomeFactory.getInstance().createWarning(
						Messages.getString(RELATIONAL.validate_warning_noColumnsDefined) ); 
				return theOutcome;
			}
		}
		
		return OutcomeFactory.getInstance().createOK();
	}
	
    /**
     * Schema validation
     * @param relationalSchema the Schema
     * @return the validation status
     */
	private IOutcome validateSchema(Schema relationalSchema) {
		return OutcomeFactory.getInstance().createOK();
	}
	
    /**
     * View validation
     * @param relationalView the View
     * @return the validation status
     */
	private IOutcome validateView(View relationalView) {
    	IOutcome theOutcome = null;
    	
		// Check Column Status values
		for( Column col : relationalView.getColumns() ) {
			theOutcome = validate(col);
			if( theOutcome.getLevel() == Level.ERROR ) {
				return theOutcome;
			}
		}
		
		// Check Column Status values
		for( Column outerColumn : relationalView.getColumns() ) {
			for( Column innerColumn : relationalView.getColumns() ) {
				if( outerColumn != innerColumn ) {
					if( outerColumn.getName().equalsIgnoreCase(innerColumn.getName())) {
						theOutcome = OutcomeFactory.getInstance().createError(
								Messages.getString(RELATIONAL.validate_error_duplicateColumnNamesInTable, relationalView.getName()) );
						return theOutcome;
					}
				}
			}
		}
		
		if( relationalView.getColumns().isEmpty() ) {
			if( relationalView.getParent() != null && relationalView.getParent() instanceof Procedure ) {
				theOutcome = OutcomeFactory.getInstance().createWarning(
						Messages.getString(RELATIONAL.validate_warning_noColumnsDefinedForResultSet) ); 
				return theOutcome;
			} else {
				theOutcome = OutcomeFactory.getInstance().createWarning(
						Messages.getString(RELATIONAL.validate_warning_noColumnsDefined) ); 
				return theOutcome;
			}
		}
		
		return OutcomeFactory.getInstance().createOK();
	}
	
    /**
     * UniqueConstraint validation
     * @param relationalUC the UniqueConstraint
     * @return the validation status
     */
	private IOutcome validateUniqueConstraint(UniqueConstraint relationalUC) {
    	IOutcome theOutcome = null;
    	
		if( relationalUC.getColumns().isEmpty() ) {
			theOutcome = OutcomeFactory.getInstance().createError(
					Messages.getString(RELATIONAL.validate_error_ucNoColumnsDefined, relationalUC.getName()) );
			return theOutcome;
		}
		
		return OutcomeFactory.getInstance().createOK();
	}

}
