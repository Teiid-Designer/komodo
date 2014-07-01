/*
 * JBoss, Home of Professional Open Source.
* See the COPYRIGHT.txt file distributed with this work for information
* regarding copyright ownership. Some portions may be licensed
* to Red Hat, Inc. under one or more contributor license agreements.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
 */
package org.komodo.relational.model;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Properties;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.RELATIONAL;
import org.komodo.relational.core.RelationalStringNameValidator;
import org.komodo.spi.outcome.IOutcome;
import org.komodo.spi.outcome.IOutcome.Level;
import org.komodo.spi.outcome.OutcomeFactory;
import org.komodo.utils.HashCodeUtil;
import org.komodo.utils.StringUtil;
import org.komodo.utils.StringUtilities;



/**
 * 
 *
 *
 */
public class Procedure extends RelationalObject {

    @SuppressWarnings("javadoc")
	public static final String KEY_FUNCTION = "FUNCTION"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
	public static final String KEY_UPDATE_COUNT = "UPDATECOUNT"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final boolean DEFAULT_FUNCTION = false;
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_UPDATE_COUNT = "AUTO"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_DATATYPE = "string"; //$NON-NLS-1$
    
    private boolean function = false;
    private boolean nonPrepared = false;
    private boolean deterministic = false;
    private boolean returnsNullOnNull = false;
    private boolean variableArguments = false;
    private boolean aggregate = false;
    private boolean allowsDistinct = false;
    private boolean allowsOrderBy = false;
    private boolean analytic = false;
    private boolean decomposable = false;
    private boolean useDistinctRows = false;
    private String nativeQuery;
    private String javaClass;
    private String javaMethod;
    private String udfJarPath;
    private String functionCategory;
    boolean sourceFunction;
    
    private String  updateCount;
    private List<Parameter> parameters;
    private ProcedureResultSet resultSet;
    
    /**
     * RelationalProcedure constructor
     */
    public Procedure() {
        super();
        this.parameters = new ArrayList<Parameter>();
        setNameValidator(new RelationalStringNameValidator(true, true));
    }
    
    /**
     * RelationalProcedure constructor
     * @param name the procedure name
     */
    public Procedure( String name ) {
        super(name);
        this.parameters = new ArrayList<Parameter>();
        setNameValidator(new RelationalStringNameValidator(true, true));
    }

    /**
     * @return type
     */
    @Override
	public int getType() {
        return TYPES.PROCEDURE;
    }

    /**
     * @return updateCount
     */
    public String getUpdateCount() {
        return updateCount;
    }
    /**
     * @param updateCount Sets updateCount to the specified value.
     */
    public void setUpdateCount( String updateCount ) {
    	if(this.updateCount!=updateCount) {
            this.updateCount = updateCount;
    		handleInfoChanged();
    	}
    }
    
    /**
     * @return function
     */
    public boolean isNonPrepared() {
        return nonPrepared;
    }
    /**
     * @param nonPrepared Sets non-prepared to the specified value.
     */
    public void setNonPrepared( boolean nonPrepared ) {
    	if(this.nonPrepared!=nonPrepared) {
            this.nonPrepared = nonPrepared;
    		handleInfoChanged();
    	}
    }
    
    /**
     * @return function
     */
    public boolean isFunction() {
        return function;
    }
    /**
     * @param function Sets function to the specified value.
     */
    public void setFunction( boolean function ) {
    	if(this.function!=function) {
            this.function = function;
    		handleInfoChanged();
    	}
    }

    /**
     * @return deterministic
     */
    public boolean isDeterministic() {
        return deterministic;
    }
    /**
     * @param deterministic Sets deterministic to the specified value.
     */
    public void setDeterministic( boolean deterministic ) {
    	if(this.deterministic!=deterministic) {
            this.deterministic = deterministic;
    		handleInfoChanged();
    	}
    }
    
    /**
     * @return returnsNullOnNull
     */
    public boolean isReturnsNullOnNull() {
        return returnsNullOnNull;
    }
    /**
     * @param returnsNullOnNull Sets returnsNullOnNull to the specified value.
     */
    public void setReturnsNullOnNull( boolean returnsNullOnNull ) {
    	if(this.returnsNullOnNull!=returnsNullOnNull) {
            this.returnsNullOnNull = returnsNullOnNull;
    		handleInfoChanged();
    	}
    }
    
    /**
     * @return variableArguments
     */
    public boolean isVariableArguments() {
        return variableArguments;
    }
    /**
     * @param variableArguments Sets variableArguments to the specified value.
     */
    public void setVariableArguments( boolean variableArguments ) {
    	if(this.variableArguments!=variableArguments) {
            this.variableArguments = variableArguments;
    		handleInfoChanged();
    	}
    }
    
    /**
     * @return aggregate
     */
    public boolean isAggregate() {
        return aggregate;
    }
    /**
     * @param aggregate Sets aggregate to the specified value.
     */
    public void setAggregate( boolean aggregate ) {
    	if(this.aggregate!=aggregate) {
    		this.aggregate = aggregate;
    		handleInfoChanged();
    	}
    }
    
    /**
     * @return allowsDistinct
     */
    public boolean isAllowsDistinct() {
        return allowsDistinct;
    }
    /**
     * @param allowsDistinct Sets allowsDistinct to the specified value.
     */
    public void setAllowsDistinct( boolean allowsDistinct ) {
    	if(this.allowsDistinct!=allowsDistinct) {
            this.allowsDistinct = allowsDistinct;
    		handleInfoChanged();
    	}
    }
    
    /**
     * @return allowsOrderBy
     */
    public boolean isAllowsOrderBy() {
        return allowsOrderBy;
    }
    /**
     * @param allowsOrderBy Sets allowsOrderBy to the specified value.
     */
    public void setAllowsOrderBy( boolean allowsOrderBy ) {
    	if(this.allowsOrderBy!=allowsOrderBy) {
            this.allowsOrderBy = allowsOrderBy;
    		handleInfoChanged();
    	}
    }
    
    /**
     * @return analytic
     */
    public boolean isAnalytic() {
        return analytic;
    }
    /**
     * @param analytic Sets analytic to the specified value.
     */
    public void setAnalytic( boolean analytic ) {
    	if(this.analytic!=analytic) {
    		this.analytic = analytic;
    		handleInfoChanged();
    	}
    }
    
    /**
     * @return decomposable
     */
    public boolean isDecomposable() {
        return decomposable;
    }
    /**
     * @param decomposable Sets decomposable to the specified value.
     */
    public void setDecomposable( boolean decomposable ) {
    	if(this.decomposable!=decomposable) {
    		this.decomposable = decomposable;
   		 	handleInfoChanged();
    	}
    }
    
    /**
     * @return useDistinctRows
     */
    public boolean isUseDistinctRows() {
        return useDistinctRows;
    }
    /**
     * @param useDistinctRows Sets useDistinctRows to the specified value.
     */
    public void setUseDistinctRows( boolean useDistinctRows ) {
    	if(this.useDistinctRows!=useDistinctRows) {
            this.useDistinctRows = useDistinctRows;
    		handleInfoChanged();
    	}
    }
    
    /**
     * @return is source function
     */
    public boolean isSourceFunction() {
        return sourceFunction;
    }
    /**
     * @param isSourceFunction Sets sourceFunction to the specified value.
     */
    public void setSourceFunction( boolean isSourceFunction ) {
    	if(this.sourceFunction!=isSourceFunction) {
            this.sourceFunction = isSourceFunction;
    		handleInfoChanged();
    	}
    }
    
    /**
     * @return java class name for function may be null
     */
    public String getJavaClassName() {
        return javaClass;
    }
    
    /**
     * @param javaClassName sets java class name to the specified value. may be null
     */
    public void setJavaClassName( String javaClassName ) {
    	if( StringUtilities.areDifferent(this.javaClass, javaClassName) ) {
	        this.javaClass = javaClassName;
    		 handleInfoChanged();
    	}
    }
    
    /**
     * @return java class name for function may be null
     */
    public String getJavaMethodName() {
        return javaMethod;
    }
    
    /**
     * @param javaMethodName sets java method name to the specified value. may be null
     */
    public void setJavaMethodName( String javaMethodName ) {
    	if( StringUtilities.areDifferent(this.javaMethod, javaMethodName) ) {
	        this.javaMethod = javaMethodName;
    		 handleInfoChanged();
    	}
    }
    
    /**
     * @return udf jar path
     */
    public String getUdfJarPath() {
        return udfJarPath;
    }
    
    /**
     * @param udfJarPath sets relative udf jar path. may be null
     */
    public void setUdfJarPath( String udfJarPath ) {
    	if( StringUtilities.areDifferent(this.udfJarPath, udfJarPath) ) {
	        this.udfJarPath = udfJarPath;
    		 handleInfoChanged();
    	}
    }
    
    /**
     * @return function category
     */
    public String getFunctionCategory() {
        return functionCategory;
    }
    
    /**
     * @param category sets user defined function category. may be null
     */
    public void setFunctionCategory( String category ) {
    	if( StringUtilities.areDifferent(this.functionCategory, category) ) {
	        this.functionCategory = category;
    		 handleInfoChanged();
    	}
    }
    
    /**
     * @return nativeQuery may be null
     */
    public String getNativeQuery() {
        return nativeQuery;
    }
    
    /**
     * @param newQuery sets nativeQuery to the specified value. may be null
     */
    public void setNativeQuery( String newQuery ) {
    	if( StringUtilities.areDifferent(this.nativeQuery, newQuery) ) {
    		this.nativeQuery = newQuery;
    		handleInfoChanged();
    	}
    }
    
    
    /**
     * @return resultSet
     */
    public ProcedureResultSet getResultSet() {
        return resultSet;
    }
    /**
     * @param resultSet Sets resultSet to the specified value.
     */
    public void setResultSet( ProcedureResultSet resultSet ) {
    	if( this.resultSet != null ) {
    		this.resultSet.setParent(null);
    	}
        this.resultSet = resultSet;
        if( this.resultSet != null) {
        	this.resultSet.setParent(this);
        }
    }
    /**
     * @return parameters
     */
    public List<Parameter> getParameters() {
        return this.parameters;
    }
    
    /**
     * @param parameter the new parameter
     */
    public void addParameter(Parameter parameter) {
        if( this.parameters.add(parameter) ) {
        	parameter.setParent(this);
    		handleInfoChanged();
        }
    }
    
    /**
     * @param parameter the parameter to remove
     * @return if parameter was removed or not
     */
    public boolean removeParameter(Parameter parameter) {
    	if( this.parameters.remove(parameter) ) {
    		handleInfoChanged();
    		return true;
    	}
    	return false;
    }
    
    /**
     * @return the new RelationalParameter
     */
    public Parameter createParameter() {
    	return createParameter(DEFAULT_DATATYPE, Parameter.DEFAULT_STRING_LENGTH);
    }
    
    /**
     * @param datatype the datatype name
     * @param length the datatype length
     * @return the new RelationalParameter
     */
    public Parameter createParameter(String datatype, int length) {
    	return createParameter("newParameter_" + (getParameters().size() + 1), datatype, length); //$NON-NLS-1$
    }
    
    /**
     * @param name the name of the parameter
     * @param datatype the datatype name
     * @param length the datatype length
     * @return the new RelationalParameter
     */
    public Parameter createParameter(String name, String datatype, int length) {
    	Parameter newParameter = new Parameter(name);
    	newParameter.setDatatype(datatype);
    	newParameter.setLength(length);
    	addParameter(newParameter);
    	return newParameter;
    }
    
	/**
	 * @param parameter the parameter
	 * @return if parameter can be moved up in child list
	 */
	public boolean canMoveParameterUp(Parameter parameter) {
		return getParameterIndex(parameter) > 0;
	}
	
	/**
	 * @param parameter the parameter
	 * @return if parameter can be moved down in child list
	 */
	public boolean canMoveParameterDown(Parameter parameter) {
		return getParameterIndex(parameter) < getParameters().size()-1;
	}
	
	private int getParameterIndex(Parameter parameter) {
		int i=0;
		for( Parameter existingParameter : getParameters() ) {
			if( existingParameter == parameter) {
				return i;
			}
			i++;
		}
		
		// Shouldn't ever get here!
		return -1;
	}
	
	/**
	 * @param theParameter the parameter
	 */
	public void moveParameterUp(Parameter theParameter) {
		int startIndex = getParameterIndex(theParameter);
		if( startIndex > 0 ) {
			// Make Copy of List & get parameterInfo of startIndex-1
			Parameter[] existingParameters = getParameters().toArray(new Parameter[0]);
			Parameter priorParameter = existingParameters[startIndex-1];
			existingParameters[startIndex-1] = theParameter;
			existingParameters[startIndex] = priorParameter;
			
			List<Parameter> newParameters = new ArrayList<Parameter>(existingParameters.length);
			for( Parameter info : existingParameters) {
				newParameters.add(info);
			}
			
			this.parameters = newParameters;
		}
	}
	
	/**
	 * @param theParameter the parameter
	 */
	public void moveParameterDown(Parameter theParameter) {
		int startIndex = getParameterIndex(theParameter);
		if( startIndex < (getParameters().size()-1) ) {
			// Make Copy of List & get parameterInfo of startIndex+1
			Parameter[] existingParameters = getParameters().toArray(new Parameter[0]);
			Parameter afterParameter = existingParameters[startIndex+1];
			existingParameters[startIndex+1] = theParameter;
			existingParameters[startIndex] = afterParameter;
			
			List<Parameter> newParameters = new ArrayList<Parameter>(existingParameters.length);
			for( Parameter info : existingParameters) {
				newParameters.add(info);
			}
			
			this.parameters = newParameters;
		}
	}
    
    /**
     * @param props the properties
     */
    @Override
	public void setProperties(Properties props) {
    	// Set common properties
    	super.setProperties(props);
    	
        for( Object key : props.keySet() ) {
            String keyStr = (String)key;
            String value = props.getProperty(keyStr);

            if( value != null && value.length() == 0 ) {
                continue;
            }
            
            if(keyStr.equalsIgnoreCase(KEY_FUNCTION) ) {
                setFunction(Boolean.parseBoolean(value));
            } else if(keyStr.equalsIgnoreCase(KEY_UPDATE_COUNT) ) {
                setUpdateCount(value);
            }
        }
    	
        handleInfoChanged();
    }
    
	@Override
	public void handleInfoChanged() {
		super.handleInfoChanged();
		
		// Set extension properties here??
		
		if( this.isFunction() ) {
			if( this.functionCategory != null ) {
				getExtensionProperties().put(FUNCTION_CATEGORY, this.functionCategory );
			} else getExtensionProperties().remove(FUNCTION_CATEGORY);
			
			if( this.javaClass != null ) {
				getExtensionProperties().put(JAVA_CLASS, this.javaClass );
			} else getExtensionProperties().remove(JAVA_CLASS);
			
			if( this.javaMethod != null ) {
				getExtensionProperties().put(JAVA_METHOD, this.javaMethod );
			} else getExtensionProperties().remove(JAVA_METHOD);
			
			if( this.udfJarPath != null ) {
				getExtensionProperties().put(UDF_JAR_PATH, this.udfJarPath );
			} else getExtensionProperties().remove(UDF_JAR_PATH);
			
			getExtensionProperties().put(AGGREGATE, Boolean.toString(this.isAggregate()) );
			getExtensionProperties().put(VARARGS, Boolean.toString(this.isVariableArguments()) );
			getExtensionProperties().put(DETERMINISTIC, Boolean.toString(this.isDeterministic()) );
			getExtensionProperties().put(NULL_ON_NULL, Boolean.toString(this.isReturnsNullOnNull()) );
			
			// If aggregate == FALSE 
			if( this.isAggregate() ) {
				getExtensionProperties().remove(ANALYTIC);
				getExtensionProperties().remove(ALLOWS_ORDER_BY);
				getExtensionProperties().remove(USES_DISTINCT_ROWS);
				getExtensionProperties().remove(DECOMPOSABLE);
				getExtensionProperties().remove(ALLOWS_DISTINCT);
			} else {
				getExtensionProperties().put(ANALYTIC, Boolean.toString(this.isAnalytic()));
				getExtensionProperties().put(ALLOWS_ORDER_BY, Boolean.toString(this.isAllowsOrderBy()));
				getExtensionProperties().put(USES_DISTINCT_ROWS, Boolean.toString(this.isUseDistinctRows()));
				getExtensionProperties().put(DECOMPOSABLE, Boolean.toString(this.isDecomposable()));
				getExtensionProperties().put(ALLOWS_DISTINCT, Boolean.toString(this.isAllowsDistinct()));
			}
			
			getExtensionProperties().remove(NATIVE_QUERY);
		} else {
			if( this.nativeQuery != null ) {
				getExtensionProperties().put(NATIVE_QUERY, this.nativeQuery );
			} else getExtensionProperties().remove(NATIVE_QUERY);
			
			getExtensionProperties().put(NON_PREPARED, Boolean.toString(this.isNonPrepared()));
			
	        // make sure model object does not have these extension properties for when function is false
			getExtensionProperties().remove(DETERMINISTIC);
			getExtensionProperties().remove(JAVA_CLASS);
			getExtensionProperties().remove(JAVA_METHOD);
			getExtensionProperties().remove(FUNCTION_CATEGORY);
			getExtensionProperties().remove(UDF_JAR_PATH);
			getExtensionProperties().remove(VARARGS);
			getExtensionProperties().remove(NULL_ON_NULL);
			getExtensionProperties().remove(AGGREGATE);
			getExtensionProperties().remove(ANALYTIC);
			getExtensionProperties().remove(ALLOWS_ORDER_BY);
			getExtensionProperties().remove(USES_DISTINCT_ROWS);
			getExtensionProperties().remove(DECOMPOSABLE);
			getExtensionProperties().remove(ALLOWS_DISTINCT);
		}
	} 
    
	@Override
	public IOutcome validate() {
		// Walk through the properties for the table and set the status
		this.currentOutcome = super.validate();
		
		// Validate Children
		for( Parameter param : getParameters() ) {
			param.validate();
		}
		
		if( getOutcome().getLevel() == Level.ERROR ) {
			return this.currentOutcome;
		}
		
		// Check Column Status values
		for( Parameter param : getParameters() ) {
			if( param.getOutcome().getLevel() == Level.ERROR ) {
				this.currentOutcome = OutcomeFactory.getInstance().createError(param.getOutcome().getMessage() );
				return this.currentOutcome;
			}
		}
		
		// Check Column Status values
		for( Parameter outerParam : getParameters() ) {
			for( Parameter innerParam : getParameters() ) {
				if( outerParam != innerParam ) {
					if( outerParam.getName().equalsIgnoreCase(innerParam.getName())) {
						this.currentOutcome = OutcomeFactory.getInstance().createError(
								Messages.getString(RELATIONAL.validate_error_duplicateParameterNamesInProcedure, getName()) ); 
						return this.currentOutcome;
					}
				}
			}
		}
		
		if( this.getParameters().isEmpty() ) {
			this.currentOutcome = OutcomeFactory.getInstance().createWarning( 
					Messages.getString(RELATIONAL.validate_warning_noParametersDefined) ); 
			return this.currentOutcome;
		}
		
		// Check for more than one RETURN parameter if Function
		if( this.isFunction() ) {
			boolean foundResultParam = false;
			for( Parameter param : getParameters() ) {
				if( param.getDirection().equalsIgnoreCase(DIRECTION.RETURN)) {
					if( foundResultParam ) {
						this.currentOutcome = OutcomeFactory.getInstance().createError(
								Messages.getString(RELATIONAL.validate_error_tooManyResultParametersInFunction) ); 
						return this.currentOutcome;
					} else {
						foundResultParam = true;
					}
				}
			}
			
			if( this.isSourceFunction() ) {
				if( getResultSet() != null ) {
					this.currentOutcome = OutcomeFactory.getInstance().createError(
							Messages.getString(RELATIONAL.validate_noResultSetAllowedInFunction) ); 
					return this.currentOutcome;
				}
			} else {
				// Check for null category, class or method name
				if( this.functionCategory == null || this.functionCategory.trim().length() == 0 ) {
					this.currentOutcome = OutcomeFactory.getInstance().createError(
							Messages.getString(RELATIONAL.validate_categoryUndefinedForUDF) ); 
					return this.currentOutcome;
				}
				if( this.javaClass == null || this.javaClass.trim().length() == 0 ) {
					this.currentOutcome = OutcomeFactory.getInstance().createError(
							Messages.getString(RELATIONAL.validate_javaClassUndefinedForUDF) ); 
					return this.currentOutcome;
				}
				if( this.javaMethod == null || this.javaMethod.trim().length() == 0 ) {
					this.currentOutcome = OutcomeFactory.getInstance().createError(
							Messages.getString(RELATIONAL.validate_javaMethodUndefinedForUDF) ); 
					return this.currentOutcome;
				}
			}
		} else {
			if( getResultSet() != null ) {
				if( getResultSet().getOutcome().getLevel() == Level.ERROR ) {
					this.currentOutcome = OutcomeFactory.getInstance().createError(getResultSet().getOutcome().getMessage() );
					return this.currentOutcome;
				}
				
				if( getResultSet().getOutcome().getLevel() == Level.WARNING ) {
					this.currentOutcome = OutcomeFactory.getInstance().createError(getResultSet().getOutcome().getMessage() );
					return this.currentOutcome;
				}
			}
		}
		return this.currentOutcome;
	}
	
    /**
     * {@inheritDoc}
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals( final Object object ) {
		if (!super.equals(object)) {
			return false;
		}
        if (this == object)
            return true;
        if (object == null)
            return false;
        if (getClass() != object.getClass())
            return false;
        final Procedure other = (Procedure)object;

        // string properties
        if (!StringUtil.valuesAreEqual(getNativeQuery(), other.getNativeQuery()) ||
        		!StringUtil.valuesAreEqual(getFunctionCategory(), other.getFunctionCategory()) ||
        		!StringUtil.valuesAreEqual(getJavaClassName(), other.getJavaClassName()) || 
        		!StringUtil.valuesAreEqual(getJavaMethodName(), other.getJavaMethodName()) ||
        		!StringUtil.valuesAreEqual(getUdfJarPath(), other.getUdfJarPath()) ||
        		!StringUtil.valuesAreEqual(getUpdateCount(), other.getUpdateCount()) ) {
            return false;
        }
        
        if( !(isAggregate()==other.isAggregate()) ||  
            !(isAllowsDistinct()==other.isAllowsDistinct()) ||
            !(isAllowsOrderBy()==other.isAllowsOrderBy()) ||
            !(isAnalytic()==other.isAnalytic()) ||
            !(isDecomposable()==other.isDecomposable()) ||
            !(isDeterministic()==other.isDeterministic()) ||
            !(isFunction()==other.isFunction()) ||
            !(isNonPrepared()==other.isNonPrepared()) ||
            !(isReturnsNullOnNull()==other.isReturnsNullOnNull()) ||
            !(isSourceFunction()==other.isSourceFunction()) ||
            !(isUseDistinctRows()==other.isUseDistinctRows()) ||
            !(isVariableArguments()==other.isVariableArguments()) ) {
        	return false;
        }
        
        // ResultSet
        if (resultSet == null) {
            if (other.resultSet != null)
                return false;   
        } else if (!resultSet.equals(other.resultSet))
            return false;
                
        // Parameters
        Collection<Parameter> thisParameters = getParameters();
        Collection<Parameter> thatParameters = other.getParameters();

        if (thisParameters.size() != thatParameters.size()) {
            return false;
        }
        
        if (!thisParameters.isEmpty() && !thisParameters.equals(thatParameters)) {
            return false;
        }
        
        return true;
    }
    
    /**
     * {@inheritDoc}
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        int result = super.hashCode();

        // string properties
        if (!StringUtil.isEmpty(getNativeQuery())) {
            result = HashCodeUtil.hashCode(result, getNativeQuery());
        }
        if (!StringUtil.isEmpty(getFunctionCategory())) {
            result = HashCodeUtil.hashCode(result, getFunctionCategory());
        }
        if (!StringUtil.isEmpty(getJavaClassName())) {
            result = HashCodeUtil.hashCode(result, getJavaClassName());
        }
        if (!StringUtil.isEmpty(getJavaMethodName())) {
            result = HashCodeUtil.hashCode(result, getJavaMethodName());
        }
        if (!StringUtil.isEmpty(getUdfJarPath())) {
            result = HashCodeUtil.hashCode(result, getUdfJarPath());
        }
        if (!StringUtil.isEmpty(getUpdateCount())) {
            result = HashCodeUtil.hashCode(result, getUpdateCount());
        }

		result = HashCodeUtil.hashCode(result, isAggregate());
        result = HashCodeUtil.hashCode(result, isAllowsDistinct());
        result = HashCodeUtil.hashCode(result, isAllowsOrderBy());
        result = HashCodeUtil.hashCode(result, isAnalytic());
		result = HashCodeUtil.hashCode(result, isDecomposable());
        result = HashCodeUtil.hashCode(result, isDeterministic());
        result = HashCodeUtil.hashCode(result, isFunction());
        result = HashCodeUtil.hashCode(result, isNonPrepared());
		result = HashCodeUtil.hashCode(result, isReturnsNullOnNull());
        result = HashCodeUtil.hashCode(result, isSourceFunction());
        result = HashCodeUtil.hashCode(result, isUseDistinctRows());
        result = HashCodeUtil.hashCode(result, isVariableArguments());
        
        if(resultSet!=null) {
            result = HashCodeUtil.hashCode(result, resultSet);
        }

        List<Parameter> params = getParameters();
        for(Parameter param: params) {
            result = HashCodeUtil.hashCode(result, param);
        }

        return result;
    }    
	
}
