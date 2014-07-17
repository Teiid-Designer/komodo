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
 * RelationalObjectValidator.  This validation returns ALL errors and warnings that is found for the RelationalObject and its
 * children.  
 */
public class RelationalObjectValidator implements RelationalValidator {

	StringNameValidator nameValidator = new RelationalStringNameValidator(false);
	DataTypeValidator dataTypeValidator = new DefaultDataTypeValidator();
	
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
	 * @see org.komodo.relational.core.RelationalValidator#setDataTypeValidator(org.komodo.relational.core.DataTypeValidator)
	 */
	@Override
	public void setDataTypeValidator(DataTypeValidator datatypeValidator) {
		this.dataTypeValidator=datatypeValidator;
	}
	
	/* (non-Javadoc)
	 * @see org.komodo.relational.core.RelationalValidator#validate(org.komodo.relational.model.RelationalObject)
	 */
    @Override
	public IOutcome validate(RelationalObject relationalObj) {
		IOutcome multiOutcome = OutcomeFactory.getInstance().createOK("Object Validation Successful"); //$NON-NLS-1$
    	
    	// --------------------------------------------------
    	// Name Validation - done for all Relational Objects
    	// --------------------------------------------------
		if( relationalObj.getName() == null || relationalObj.getName().length() == 0 ) {
		    multiOutcome.addOutcome(OutcomeFactory.getInstance().createError(
					  Messages.getString(RELATIONAL.validate_error_nameCannotBeNullOrEmpty, relationalObj.getDisplayName()) ));
		}
		if(multiOutcome.isOK()) {
			// Validate non-null string
			String errorMessage = getNameValidator().checkValidName(relationalObj.getName());
			if( errorMessage != null && !errorMessage.isEmpty() ) {
				multiOutcome.addOutcome(OutcomeFactory.getInstance().createError(errorMessage));
			}
		}
		
    	// --------------------------------------------------
    	// Object-specific validation rules
    	// --------------------------------------------------
		int objType = relationalObj.getType();
		switch(objType) {
		case TYPES.TABLE: 
			return validateTable((Table)relationalObj,multiOutcome);
		case TYPES.COLUMN:
			return validateColumn((Column)relationalObj,multiOutcome);
		case TYPES.PROCEDURE:
			return validateProcedure((Procedure)relationalObj,multiOutcome);
		case TYPES.PARAMETER:
			return validateParameter((Parameter)relationalObj,multiOutcome);
		case TYPES.RESULT_SET:
			return validateProcedureResultSet((ProcedureResultSet)relationalObj,multiOutcome);
		case TYPES.SCHEMA:
			return validateSchema((Schema)relationalObj,multiOutcome);
		case TYPES.VIEW:
			return validateView((View)relationalObj,multiOutcome);
		case TYPES.UC:
			return validateUniqueConstraint((UniqueConstraint)relationalObj,multiOutcome);
		case TYPES.AP:
			return validateAccessPattern((AccessPattern)relationalObj,multiOutcome);
		case TYPES.PK:
			return validatePrimaryKey((PrimaryKey)relationalObj,multiOutcome);
		case TYPES.FK:
			return validateForeignKey((ForeignKey)relationalObj,multiOutcome);
		case TYPES.INDEX:
			return validateIndex((Index)relationalObj,multiOutcome);
		case TYPES.MODEL:
			return validateModel((Model)relationalObj,multiOutcome);
		}
		
		return multiOutcome;
    }
    
    /**
     * Table validation
     * @param relationalTable the table
     * @return the validation status
     */
	private IOutcome validateTable(Table relationalTable, IOutcome multiOutcome) {
		if( relationalTable.isMaterialized() && relationalTable.getMaterializedTable() == null ) {
			multiOutcome.addOutcome(OutcomeFactory.getInstance().createError(
					Messages.getString(RELATIONAL.validate_error_materializedTableHasNoTableDefined) ));
		}
		
		if( relationalTable.getPrimaryKey() != null) {
			IOutcome outcome = validate(relationalTable.getPrimaryKey());
			if(!outcome.isOK()) {
				multiOutcome.addOutcome(outcome);
			}
		}
		
		if( relationalTable.getUniqueContraint() != null ) {
			IOutcome outcome = validate(relationalTable.getUniqueContraint());
			if(!outcome.isOK()) {
				multiOutcome.addOutcome(outcome);
			}
		}
		
		for( ForeignKey fk : relationalTable.getForeignKeys() ) {
			IOutcome outcome = validate(fk);
			if(!outcome.isOK()) {
				multiOutcome.addOutcome(outcome);
			}
		}
		
		// Check Column Status values
		for( Column col : relationalTable.getColumns() ) {
			IOutcome outcome = validate(col);
			if(!outcome.isOK()) {
				multiOutcome.addOutcome(outcome);
			}
		}
		
		// Check Column Status values
		for( Column outerColumn : relationalTable.getColumns() ) {
			for( Column innerColumn : relationalTable.getColumns() ) {
				if( outerColumn != innerColumn ) {
					if( outerColumn.getName().equalsIgnoreCase(innerColumn.getName())) {
						multiOutcome.addOutcome(OutcomeFactory.getInstance().createError(
								Messages.getString(RELATIONAL.validate_error_duplicateColumnNamesInTable, relationalTable.getName()) ));
						break;
					}
				}
			}
		}
		
		if( relationalTable.getColumns().isEmpty() ) {
			if( relationalTable.getParent() != null && relationalTable.getParent() instanceof Procedure ) {
				multiOutcome.addOutcome(OutcomeFactory.getInstance().createWarning(
						Messages.getString(RELATIONAL.validate_warning_noColumnsDefinedForResultSet) )); 
			} else {
				multiOutcome.addOutcome(OutcomeFactory.getInstance().createWarning(
						Messages.getString(RELATIONAL.validate_warning_noColumnsDefined) )); 
			}
		}
		
		return multiOutcome;
	}
	
    /**
     * Column validation
     * @param relationalColumn the column
     * @return the validation status
     */
	private IOutcome validateColumn(Column relationalColumn, IOutcome multiOutcome) {
		IOutcome outcome = this.dataTypeValidator.validate(relationalColumn.getDatatype());
		if(!outcome.isOK()) {
			multiOutcome.addOutcome(outcome);
		}
		return multiOutcome;
	}
	
    /**
     * AccessPattern validation
     * @param relationalAP the access pattern
     * @return the validation status
     */
	private IOutcome validateAccessPattern(AccessPattern relationalAP, IOutcome multiOutcome) {
		return multiOutcome;
	}
	
    /**
     * ForeignKey validation
     * @param relationalFK the foreign key
     * @return the validation status
     */
	private IOutcome validateForeignKey(ForeignKey relationalFK, IOutcome multiOutcome) {
		
		if( relationalFK.getColumns().isEmpty() ) {
			multiOutcome.addOutcome(OutcomeFactory.getInstance().createError(
					Messages.getString(RELATIONAL.validate_error_fkNoColumnsDefined, relationalFK.getName()) ));
		}
				
		if( relationalFK.getUniqueKeyName() == null || relationalFK.getUniqueKeyName().length() == 0 ) {
			multiOutcome.addOutcome(OutcomeFactory.getInstance().createError(
					Messages.getString(RELATIONAL.validate_error_fKUniqueKeyNameIsUndefined, relationalFK.getName()) ));
		}
		
		if( relationalFK.getUniqueKeyTableName() == null || relationalFK.getUniqueKeyTableName().length() == 0 ) {
			multiOutcome.addOutcome(OutcomeFactory.getInstance().createError(
					Messages.getString(RELATIONAL.validate_error_fKReferencedUniqueKeyTableIsUndefined) ));
		}
    	
		return multiOutcome;
	}
	
    /**
     * PrimaryKey validation
     * @param relationalPK the Primary Key
     * @return the validation status
     */
	private IOutcome validatePrimaryKey(PrimaryKey relationalPK, IOutcome multiOutcome) {
    	
		if( relationalPK.getColumns().isEmpty() ) {
			multiOutcome.addOutcome(OutcomeFactory.getInstance().createError(
					Messages.getString(RELATIONAL.validate_error_pkNoColumnsDefined, relationalPK.getName()) ));
		}

		return multiOutcome;
	}
	
    /**
     * Index validation
     * @param relationalIndex the Index
     * @return the validation status
     */
	private IOutcome validateIndex(Index relationalIndex, IOutcome multiOutcome) {
    	
		// Check Column Status values
		for( Column col : relationalIndex.getColumns() ) {
			IOutcome outcome = validate(col);
			if(!outcome.isOK()) {
				multiOutcome.addOutcome(outcome);
			}
		}
		
		// Check Column Status values
		for( Column outerColumn : relationalIndex.getColumns() ) {
			for( Column innerColumn : relationalIndex.getColumns() ) {
				if( outerColumn != innerColumn ) {
					if( outerColumn.getName().equalsIgnoreCase(innerColumn.getName())) {
						multiOutcome.addOutcome(OutcomeFactory.getInstance().createError(
								Messages.getString(RELATIONAL.validate_error_duplicateColumnNamesReferencedInIndex, relationalIndex.getName()) )); 
						break;
					}
				}
			}
		}
		
		if( relationalIndex.getColumns().isEmpty() ) {
			multiOutcome.addOutcome(OutcomeFactory.getInstance().createError(
					Messages.getString(RELATIONAL.validate_warning_noColumnReferencesDefined, relationalIndex.getName()) ));
		}
    	
		return multiOutcome;
	}
	
    /**
     * Model validation
     * @param relationalModel the model
     * @return the validation status
     */
	private IOutcome validateModel(Model relationalModel, IOutcome multiOutcome) {
		return multiOutcome;
	}
	
    /**
     * Parameter validation
     * @param relationalParmeter the Parameter
     * @return the validation status
     */
	private IOutcome validateParameter(Parameter relationalParmeter, IOutcome multiOutcome) {
    	
		// Parameter directions check
		Procedure parentProcedure = (Procedure)relationalParmeter.getParent();
		if(parentProcedure!=null && parentProcedure.isFunction()) {
			if( ! relationalParmeter.getDirection().equalsIgnoreCase(DIRECTION.IN) &&
					! relationalParmeter.getDirection().equalsIgnoreCase(DIRECTION.RETURN)	) {
				multiOutcome.addOutcome(OutcomeFactory.getInstance().createError(
						Messages.getString(RELATIONAL.validate_error_invalidParameterDirectionInFunction) )); 
			}
		}
		
		IOutcome outcome = this.dataTypeValidator.validate(relationalParmeter.getDatatype());
		if(!outcome.isOK()) {
			multiOutcome.addOutcome(outcome);
		}
		return multiOutcome;
	}
	
    /**
     * Procedure validation
     * @param relationalProcedure the Procedure
     * @return the validation status
     */
	private IOutcome validateProcedure(Procedure relationalProcedure, IOutcome multiOutcome) {
    	
    	List<Parameter> params = relationalProcedure.getParameters();
    	
    	if(!params.isEmpty()) {
    		// Validate Parameters
    		for( Parameter param : params ) {
    			IOutcome outcome = validate(param);
    			if(!outcome.isOK()) {
    				multiOutcome.addOutcome(outcome);
    			}
    		}

    		// Check Parameter Status values
    		for( Parameter outerParam : params ) {
    			for( Parameter innerParam : params ) {
    				if( outerParam != innerParam ) {
    					if( outerParam.getName().equalsIgnoreCase(innerParam.getName())) {
    						multiOutcome.addOutcome(OutcomeFactory.getInstance().createError(
    								Messages.getString(RELATIONAL.validate_error_duplicateParameterNamesInProcedure, relationalProcedure.getName()) ));
    						break;
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
						multiOutcome.addOutcome(OutcomeFactory.getInstance().createError(
								Messages.getString(RELATIONAL.validate_error_tooManyResultParametersInFunction) )); 
						break;
					} else {
						foundResultParam = true;
					}
				}
			}
			
			if( relationalProcedure.isSourceFunction() ) {
				if( relationalProcedure.getResultSet() != null ) {
					multiOutcome.addOutcome(OutcomeFactory.getInstance().createError(
							Messages.getString(RELATIONAL.validate_noResultSetAllowedInFunction) )); 
				}
			} else {
				// Check for null category, class or method name
				if( relationalProcedure.getFunctionCategory() == null || relationalProcedure.getFunctionCategory().trim().length() == 0 ) {
					multiOutcome.addOutcome(OutcomeFactory.getInstance().createError(
							Messages.getString(RELATIONAL.validate_categoryUndefinedForUDF) )); 
				}
				if( relationalProcedure.getJavaClassName() == null || relationalProcedure.getJavaClassName().trim().length() == 0 ) {
					multiOutcome.addOutcome(OutcomeFactory.getInstance().createError(
							Messages.getString(RELATIONAL.validate_javaClassUndefinedForUDF) )); 
				}
				if( relationalProcedure.getJavaMethodName() == null || relationalProcedure.getJavaMethodName().trim().length() == 0 ) {
					multiOutcome.addOutcome(OutcomeFactory.getInstance().createError(
							Messages.getString(RELATIONAL.validate_javaMethodUndefinedForUDF) )); 
				}
			}
		} else {
			if( relationalProcedure.getResultSet() != null ) {
				if( relationalProcedure.getResultSet().getOutcome().getLevel() == Level.ERROR ) {
					multiOutcome.addOutcome(OutcomeFactory.getInstance().createError(relationalProcedure.getResultSet().getOutcome().getMessage() ));
				}
				
				if( relationalProcedure.getResultSet().getOutcome().getLevel() == Level.WARNING ) {
					multiOutcome.addOutcome(OutcomeFactory.getInstance().createError(relationalProcedure.getResultSet().getOutcome().getMessage() ));
				}
			}
		}
		
		if( params.isEmpty() ) {
			multiOutcome.addOutcome(OutcomeFactory.getInstance().createWarning( 
					Messages.getString(RELATIONAL.validate_warning_noParametersDefined) )); 
		}
		
		return multiOutcome;
	}
	
    /**
     * ProcedureResultSet validation
     * @param relationalProcedureResultSet the ProcedureResultSet
     * @return the validation status
     */
	private IOutcome validateProcedureResultSet(ProcedureResultSet relationalProcedureResultSet, IOutcome multiOutcome) {
    	
		// Check Column Status values
		for( Column col : relationalProcedureResultSet.getColumns() ) {
			IOutcome outcome = validate(col);
			if(!outcome.isOK()) {
				multiOutcome.addOutcome(outcome);
			}
		}
		
		// Check Column Status values
		for( Column outerColumn : relationalProcedureResultSet.getColumns() ) {
			for( Column innerColumn : relationalProcedureResultSet.getColumns() ) {
				if( outerColumn != innerColumn ) {
					if( outerColumn.getName().equalsIgnoreCase(innerColumn.getName())) {
						multiOutcome.addOutcome(OutcomeFactory.getInstance().createError(
								Messages.getString(RELATIONAL.validate_error_duplicateColumnNamesInTable, relationalProcedureResultSet.getName()) ));
						break;
					}
				}
			}
		}
		
		if( relationalProcedureResultSet.getColumns().isEmpty() ) {
			if( relationalProcedureResultSet.getParent() != null && relationalProcedureResultSet.getParent() instanceof Procedure ) {
				multiOutcome.addOutcome(OutcomeFactory.getInstance().createWarning(
						Messages.getString(RELATIONAL.validate_warning_noColumnsDefinedForResultSet) )); 
			} else {
				multiOutcome.addOutcome(OutcomeFactory.getInstance().createWarning(
						Messages.getString(RELATIONAL.validate_warning_noColumnsDefined) )); 
			}
		}
		
		return multiOutcome;
	}
	
    /**
     * Schema validation
     * @param relationalSchema the Schema
     * @return the validation status
     */
	private IOutcome validateSchema(Schema relationalSchema, IOutcome multiOutcome) {
		return multiOutcome;
	}
	
    /**
     * View validation
     * @param relationalView the View
     * @return the validation status
     */
	private IOutcome validateView(View relationalView, IOutcome multiOutcome) {
    	
		// Check Column Status values
		for( Column col : relationalView.getColumns() ) {
			IOutcome outcome = validate(col);
			if(!outcome.isOK()) {
				multiOutcome.addOutcome(outcome);
			}
		}
		
		// Check Column Status values
		for( Column outerColumn : relationalView.getColumns() ) {
			for( Column innerColumn : relationalView.getColumns() ) {
				if( outerColumn != innerColumn ) {
					if( outerColumn.getName().equalsIgnoreCase(innerColumn.getName())) {
						multiOutcome.addOutcome(OutcomeFactory.getInstance().createError(
								Messages.getString(RELATIONAL.validate_error_duplicateColumnNamesInTable, relationalView.getName()) ));
						break;
					}
				}
			}
		}
		
		if( relationalView.getColumns().isEmpty() ) {
			if( relationalView.getParent() != null && relationalView.getParent() instanceof Procedure ) {
				multiOutcome.addOutcome(OutcomeFactory.getInstance().createWarning(
						Messages.getString(RELATIONAL.validate_warning_noColumnsDefinedForResultSet) )); 
			} else {
				multiOutcome.addOutcome(OutcomeFactory.getInstance().createWarning(
						Messages.getString(RELATIONAL.validate_warning_noColumnsDefined) )); 
			}
		}
		
		return multiOutcome;
	}
	
    /**
     * UniqueConstraint validation
     * @param relationalUC the UniqueConstraint
     * @return the validation status
     */
	private IOutcome validateUniqueConstraint(UniqueConstraint relationalUC, IOutcome multiOutcome) {
    	
		if( relationalUC.getColumns().isEmpty() ) {
			multiOutcome.addOutcome(OutcomeFactory.getInstance().createError(
					Messages.getString(RELATIONAL.validate_error_ucNoColumnsDefined, relationalUC.getName()) ));
		}
		
		return multiOutcome;
	}

}
