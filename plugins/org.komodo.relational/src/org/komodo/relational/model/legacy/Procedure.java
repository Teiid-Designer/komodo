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
package org.komodo.relational.model.legacy;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import org.komodo.utils.HashCodeUtils;
import org.komodo.utils.StringUtils;



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
        getValidator().setNameValidator(new RelationalStringNameValidator(true, true));
    }
    
    /**
     * RelationalProcedure constructor
     * @param name the procedure name
     */
    public Procedure( String name ) {
        super(name);
        this.parameters = new ArrayList<Parameter>();
        getValidator().setNameValidator(new RelationalStringNameValidator(true, true));
    }

    /**
     * @return type
     */
    @Override
	public int getType() {
        return TYPES.PROCEDURE;
    }

    /**
     * Add a child to this procedure
     * @param child the child
     * @return 'true' if child was added
     */
    @Override
	public boolean addChild(RelationalObject child) {
    	int objectType = child.getType();
    	boolean success = false;
    	
		switch(objectType) {
		case TYPES.PARAMETER:
			success = addParameter((Parameter)child);
			break;
		case TYPES.RESULT_SET:
			success = setResultSet((ProcedureResultSet)child);
		}
        
        return success;
    }
    
    /**
     * Remove specified child from the procedure
     * @param child the child to remove
     * @return 'true' if child was removed
     */
    @Override
	public boolean removeChild(RelationalObject child) {
    	int objectType = child.getType();
    	boolean success = false;
    	
		switch(objectType) {
		case TYPES.PARAMETER:
			success = removeParameter((Parameter)child);
			break;
		case TYPES.RESULT_SET:
			success = setResultSet(null);
		}
        
        return success;
    }
    
    /**
     * Get the children for this Procedure
     * @return children
     */
    @Override
	public Collection<RelationalObject> getChildren() {
    	Collection<RelationalObject> children = new ArrayList<RelationalObject>();
    	
    	if(getParameters()!=null && !getParameters().isEmpty()) {
    		children.addAll(getParameters());
    	}
    	if(getResultSet()!=null) {
    		children.add(getResultSet());
    	}
    	
        return children;
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
    	if( StringUtils.areDifferent(this.javaClass, javaClassName) ) {
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
    	if( StringUtils.areDifferent(this.javaMethod, javaMethodName) ) {
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
    	if( StringUtils.areDifferent(this.udfJarPath, udfJarPath) ) {
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
    	if( StringUtils.areDifferent(this.functionCategory, category) ) {
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
    	if( StringUtils.areDifferent(this.nativeQuery, newQuery) ) {
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
     * Set the result set
     * @param resultSet Sets resultSet to the specified value.
     * @return 'true' if successfully set
     */
    public boolean setResultSet(ProcedureResultSet resultSet) {
    	boolean wasSet = false;
    	if( this.resultSet != resultSet ) {
    		if(resultSet==null) {
    			this.resultSet = resultSet;
    		} else if ( !resultSet.equals(this.resultSet) ) {
		        this.resultSet = resultSet;
	    		this.resultSet.setParent(this);
    		}
	        wasSet = true;
	        handleInfoChanged();
    	}
    	return wasSet;
    }
    
    /**
     * @return parameters
     */
    public List<Parameter> getParameters() {
        return this.parameters;
    }
    
    /**
     * Add the parameter
     * @param parameter the new parameter
     * @return 'true' if successfully added
     */
    public boolean addParameter(Parameter parameter) {
    	boolean wasAdded = false;
    	if(!this.parameters.contains(parameter)) {
        	wasAdded = this.parameters.add(parameter);
    	}
    	if( wasAdded ) {
    		if(parameter.getParent()!=this) parameter.setParent(this);
    		handleInfoChanged();
    	} 
    	return wasAdded;
    }
    
    /**
     * Remove the parameter
     * @param parameter the parameter to remove
     * @return if parameter was removed or not
     */
    public boolean removeParameter(Parameter parameter) {
    	boolean wasRemoved = this.parameters.remove(parameter);
    	if( wasRemoved ) {
    		handleInfoChanged();
    	}
    	return wasRemoved;
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
    	DataType dType = new DataType(datatype,length);
    	newParameter.setDatatype(dType);
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
     * Get the properties for this object
     * @return the properties
     */
    @Override
	public Map<String,String> getProperties() {
    	Map<String,String> props = super.getProperties();
    	
    	props.put(KEY_FUNCTION, String.valueOf(isFunction()));
    	props.put(KEY_UPDATE_COUNT, getUpdateCount());
    	
    	return props;
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
        if (!StringUtils.valuesAreEqual(getNativeQuery(), other.getNativeQuery()) ||
        		!StringUtils.valuesAreEqual(getFunctionCategory(), other.getFunctionCategory()) ||
        		!StringUtils.valuesAreEqual(getJavaClassName(), other.getJavaClassName()) || 
        		!StringUtils.valuesAreEqual(getJavaMethodName(), other.getJavaMethodName()) ||
        		!StringUtils.valuesAreEqual(getUdfJarPath(), other.getUdfJarPath()) ||
        		!StringUtils.valuesAreEqual(getUpdateCount(), other.getUpdateCount()) ) {
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
        if (!StringUtils.isEmpty(getNativeQuery())) {
            result = HashCodeUtils.hashCode(result, getNativeQuery());
        }
        if (!StringUtils.isEmpty(getFunctionCategory())) {
            result = HashCodeUtils.hashCode(result, getFunctionCategory());
        }
        if (!StringUtils.isEmpty(getJavaClassName())) {
            result = HashCodeUtils.hashCode(result, getJavaClassName());
        }
        if (!StringUtils.isEmpty(getJavaMethodName())) {
            result = HashCodeUtils.hashCode(result, getJavaMethodName());
        }
        if (!StringUtils.isEmpty(getUdfJarPath())) {
            result = HashCodeUtils.hashCode(result, getUdfJarPath());
        }
        if (!StringUtils.isEmpty(getUpdateCount())) {
            result = HashCodeUtils.hashCode(result, getUpdateCount());
        }

		result = HashCodeUtils.hashCode(result, isAggregate());
        result = HashCodeUtils.hashCode(result, isAllowsDistinct());
        result = HashCodeUtils.hashCode(result, isAllowsOrderBy());
        result = HashCodeUtils.hashCode(result, isAnalytic());
		result = HashCodeUtils.hashCode(result, isDecomposable());
        result = HashCodeUtils.hashCode(result, isDeterministic());
        result = HashCodeUtils.hashCode(result, isFunction());
        result = HashCodeUtils.hashCode(result, isNonPrepared());
		result = HashCodeUtils.hashCode(result, isReturnsNullOnNull());
        result = HashCodeUtils.hashCode(result, isSourceFunction());
        result = HashCodeUtils.hashCode(result, isUseDistinctRows());
        result = HashCodeUtils.hashCode(result, isVariableArguments());
        
        if(resultSet!=null) {
            result = HashCodeUtils.hashCode(result, resultSet);
        }

        List<Parameter> params = getParameters();
        for(Parameter param: params) {
            result = HashCodeUtils.hashCode(result, param);
        }

        return result;
    }    
	
}
