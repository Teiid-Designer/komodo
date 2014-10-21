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

import java.util.Map;
import java.util.Properties;

import org.komodo.relational.Messages;
import org.komodo.relational.Messages.RELATIONAL;
import org.komodo.relational.core.DataType;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.HashCodeUtils;
import org.komodo.utils.StringUtils;



/**
 * 
 *
 *
 */
public class Parameter extends RelationalObject {
    @SuppressWarnings("javadoc")
    public static final String KEY_DATATYPE = "DATATYPE"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_NATIVE_TYPE = "NATIVETYPE"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_NULLABLE = "NULLABLE"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_DIRECTION = "DIRECTION"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_DEFAULT_VALUE = "DEFAULTVALUE"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_LENGTH = "LENGTH"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_PRECISION = "PRECISION"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_RADIX = "RADIX"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_SCALE = "SCALE"; //$NON-NLS-1$
    
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_NATIVE_TYPE = null;
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_NULLABLE = NULLABLE.NULLABLE;
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_DIRECTION = DIRECTION.IN;
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_DEFAULT_VALUE = null;
    
    @SuppressWarnings("javadoc")
    public static final int DEFAULT_STRING_LENGTH = 4000;
    
    private String  nativeType;
    private String  nullable;
    private String  direction = DEFAULT_DIRECTION;
    private String  defaultValue;
    
    private DataType dataType = new DataType();

    /**
     * RelationalParameter constructor
     */
    public Parameter() {
        super();
    }
    
    /**
     * RelationalParameter constructor
     * @param name the parameter name
     */
    public Parameter( String name ) {
        super(name);
    }
    
    /**
     * @return type
     */
    @Override
	public int getType() {
        return TYPES.PARAMETER;
    }

    /**
     * @return datatype
     */
    public String getDatatypeName() {
        return this.dataType.getName();
    }
    
    /**
     * @param typeName Sets datatype to the specified value.
     */
    public void setDatatypeName( String typeName ) {
    	this.dataType.setName(typeName);
    }
    
    /**
     * @param datatype the datatype
     */
    public void setDatatype( DataType datatype ) {
    	ArgCheck.isNotNull(datatype);
    	this.dataType = datatype;
    }
    
    /**
     * @return the datatype
     */
    public DataType getDatatype( ) {
    	return this.dataType;
    }
    
    /**
     * @return nativeType
     */
    public String getNativeType() {
        return nativeType;
    }
    /**
     * @param nativeType Sets nativeType to the specified value.
     */
    public void setNativeType( String nativeType ) {
        this.nativeType = nativeType;
    }
    /**
     * @return nullable
     */
    public String getNullable() {
        return nullable;
    }
    /**
     * @param nullable Sets nullable to the specified value.
     */
    public void setNullable( String nullable ) {
    	ArgCheck.isNotEmpty(nullable);
    	String[] allowedValues = NULLABLE.AS_ARRAY;
    	boolean matchFound = false;
    	for(int i=0; i<allowedValues.length; i++) {
    		if(allowedValues[i].equalsIgnoreCase(nullable)) {
    			this.nullable = allowedValues[i];
    			matchFound = true;
    		}
    	}
    	if(!matchFound) throw new IllegalArgumentException(Messages.getString(RELATIONAL.columnError_Nullable_NotAllowable,nullable));
    }
    /**
     * @return direction
     */
    public String getDirection() {
        return direction;
    }
    /**
     * @param direction Sets direction to the specified value.
     */
    public void setDirection( String direction ) {
    	ArgCheck.isNotEmpty(direction);
    	String[] allowedValues = DIRECTION.AS_ARRAY;
    	boolean matchFound = false;
    	for(int i=0; i<allowedValues.length; i++) {
    		if(allowedValues[i].equalsIgnoreCase(direction)) {
    			this.direction = allowedValues[i];
    			matchFound = true;
    		}
    	}
    	if(!matchFound) throw new IllegalArgumentException(Messages.getString(RELATIONAL.parameterError_Direction_NotAllowable,direction));
    }
    /**
     * @return defaultValue
     */
    public String getDefaultValue() {
        return defaultValue;
    }
    /**
     * @param defaultValue Sets defaultValue to the specified value.
     */
    public void setDefaultValue( String defaultValue ) {
        this.defaultValue = defaultValue;
    }
    /**
     * @return length
     */
    public long getLength() {
        return this.dataType.getLength();
    }
    /**
     * @param length Sets length to the specified value.
     */
    public void setLength( long length ) {
        this.dataType.setLength(length);
    }
    /**
     * @return precision
     */
    public int getPrecision() {
        return this.dataType.getPrecision();
    }
    /**
     * @param precision Sets precision to the specified value.
     */
    public void setPrecision( int precision ) {
        this.dataType.setPrecision(precision);
    }
    /**
     * @return scale
     */
    public int getScale() {
        return this.dataType.getScale();
    }
    /**
     * @param scale Sets scale to the specified value.
     */
    public void setScale( int scale ) {
        this.dataType.setScale(scale);
    }
    /**
     * @return radix
     */
    public int getRadix() {
        return this.dataType.getRadix();
    }
    /**
     * @param radix Sets radix to the specified value.
     */
    public void setRadix( int radix ) {
        this.dataType.setRadix(radix);
    }
    
    /**
     * Get the properties for this object
     * @return the properties
     */
    @Override
	public Map<String,String> getProperties() {
    	Map<String,String> props = super.getProperties();
    	
    	props.put(KEY_LENGTH, String.valueOf(getLength()));
    	props.put(KEY_DATATYPE, getDatatypeName());
    	props.put(KEY_DEFAULT_VALUE, getDefaultValue());
    	props.put(KEY_DIRECTION, getDirection());
    	props.put(KEY_NATIVE_TYPE, getNativeType());
    	props.put(KEY_NULLABLE, getNullable());
    	props.put(KEY_PRECISION, String.valueOf(getPrecision()));
    	props.put(KEY_SCALE, String.valueOf(getScale()));
    	props.put(KEY_RADIX, String.valueOf(getRadix()));
    	
    	return props;
    }
    
    /**
     * Set properties
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
            
            if(keyStr.equalsIgnoreCase(KEY_LENGTH) ) {
                setLength(Integer.parseInt(value));
            } else if(keyStr.equalsIgnoreCase(KEY_DATATYPE) ) {
                setDatatypeName(value);
            } else if(keyStr.equalsIgnoreCase(KEY_DEFAULT_VALUE) ) {
                setDefaultValue(value);
            } else if(keyStr.equalsIgnoreCase(KEY_DIRECTION) ) {
                setDirection(value);
            } else if(keyStr.equalsIgnoreCase(KEY_NATIVE_TYPE) ) {
                setNativeType(value);
            } else if(keyStr.equalsIgnoreCase(KEY_NULLABLE) ) {
                setNullable(value);
            } else if(keyStr.equalsIgnoreCase(KEY_PRECISION) ) {
                setPrecision(Integer.parseInt(value));
            } else if(keyStr.equalsIgnoreCase(KEY_SCALE) ) {
                setScale(Integer.parseInt(value));
            } else if(keyStr.equalsIgnoreCase(KEY_RADIX) ) {
                setRadix(Integer.parseInt(value));
            } 
        }
    	
        handleInfoChanged();
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
        final Parameter other = (Parameter)object;

        // string properties
        if (!StringUtils.valuesAreEqual(getDatatypeName(), other.getDatatypeName()) ||
        		!StringUtils.valuesAreEqual(getDefaultValue(), other.getDefaultValue()) ||
        		!StringUtils.valuesAreEqual(getDirection(), other.getDirection()) ||
        		!StringUtils.valuesAreEqual(getNativeType(), other.getNativeType()) ||
        		!StringUtils.valuesAreEqual(getNullable(), other.getNullable()) ) {
        	return false;
        }
        
        if( !(getLength()==other.getLength()) ||
            !(getPrecision()==other.getPrecision()) ||
            !(getRadix()==other.getRadix()) ||
            !(getScale()==other.getScale()) ) {
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
        if (!StringUtils.isEmpty(getDatatypeName())) {
            result = HashCodeUtils.hashCode(result, getDatatype());
        }
        if (!StringUtils.isEmpty(getDefaultValue())) {
            result = HashCodeUtils.hashCode(result, getDefaultValue());
        }
        if (!StringUtils.isEmpty(getDirection())) {
            result = HashCodeUtils.hashCode(result, getDirection());
        }
        if (!StringUtils.isEmpty(getNativeType())) {
            result = HashCodeUtils.hashCode(result, getNativeType());
        }
        if (!StringUtils.isEmpty(getNullable())) {
            result = HashCodeUtils.hashCode(result, getNullable());
        }

        result = HashCodeUtils.hashCode(result, getLength());
        result = HashCodeUtils.hashCode(result, getPrecision());
        result = HashCodeUtils.hashCode(result, getRadix());
        result = HashCodeUtils.hashCode(result, getScale());

        return result;
    }    

}
