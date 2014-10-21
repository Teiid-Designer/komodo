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
 * RelationalColumn
 *
 *
 */
public class Column extends RelationalObject {
    @SuppressWarnings("javadoc")
    public static final String KEY_DISTINCT_VALUE_COUNT = "DISTINCTVALUECOUNT"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_NULL_VALUE_COUNT = "NULLVALUECOUNT"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_DATATYPE = "DATATYPE"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_NATIVE_TYPE = "NATIVETYPE"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_NULLABLE = "NULLABLE"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_AUTO_INCREMENTED = "AUTOINCREMENTED"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_CASE_SENSITIVE = "CASESENSITIVE"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_CHARACTER_SET_NAME = "CHARACTERSETNAME"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_CHARACTER_OCTET_LENGTH = "CHARACTEROCTETLENGTH"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_COLLATION_NAME = "COLLATIONNAME"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_CURRENCY = "CURRENCY"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_DEFAULT_VALUE = "DEFAULTVALUE"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_FORMAT = "FORMAT"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_LENGTH = "LENGTH"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_LENGTH_FIXED = "LENGTHFIXED"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_MAXIMUM_VALUE = "MAXIMUMVALUE"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_MINIMUM_VALUE = "MINIMUMVALUE"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_PRECISION = "PRECISION"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_SCALE = "SCALE"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_RADIX = "RADIX"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_SIGNED = "SIGNED"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_SEARCHABILITY = "SEARCHABILITY"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_SELECTABLE = "SELECTABLE"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_UPDATEABLE = "UPDATEABLE"; //$NON-NLS-1$
    
    @SuppressWarnings("javadoc")
    public static final int DEFAULT_DISTINCT_VALUE_COUNT = -1;
    @SuppressWarnings("javadoc")
    public static final int DEFAULT_NULL_VALUE_COUNT = -1;
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_DATATYPE = null;
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_NATIVE_TYPE = null;
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_NULLABLE = NULLABLE.NULLABLE;
    @SuppressWarnings("javadoc")
    public static final boolean DEFAULT_AUTO_INCREMENTED = false;
    @SuppressWarnings("javadoc")
    public static final boolean DEFAULT_CASE_SENSITIVE = true;
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_CHARACTER_SET_NAME = null;
    @SuppressWarnings("javadoc")
    public static final int DEFAULT_CHARACTER_OCTET_LENGTH = 0;
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_COLLATION_NAME = null;
    @SuppressWarnings("javadoc")
    public static final boolean DEFAULT_CURRENCY = false;
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_DEFAULT_VALUE = null;
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_FORMAT = null;
    @SuppressWarnings("javadoc")
    public static final boolean DEFAULT_LENGTH_FIXED = false;
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_MAXIMUM_VALUE = null;
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_MINIMUM_VALUE = null;
    @SuppressWarnings("javadoc")
    public static final boolean DEFAULT_SIGNED = true;
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_SEARCHABILITY = SEARCHABILITY.SEARCHABLE;
    @SuppressWarnings("javadoc")
    public static final boolean DEFAULT_SELECTABLE = true;
    @SuppressWarnings("javadoc")
    public static final boolean DEFAULT_UPDATEABLE = true;
    
    @SuppressWarnings("javadoc")
    public static final int DEFAULT_STRING_LENGTH = 4000;
    
    private int distinctValueCount = DEFAULT_DISTINCT_VALUE_COUNT;
    private int nullValueCount = DEFAULT_NULL_VALUE_COUNT;
    private String  nativeType;
    private String  nullable = DEFAULT_NULLABLE;
    private boolean autoIncremented;
    private boolean caseSensitive = DEFAULT_CASE_SENSITIVE;
    private String  characterSetName;
    private String  collationName;
    private boolean currency;
    private String  defaultValue;
    private String  format;
    private boolean lengthFixed;
    private String  maximumValue;
    private String  minimumValue;
    private int characterOctetLength;
	private boolean signed = DEFAULT_SIGNED;
    private String  searchability = DEFAULT_SEARCHABILITY;
    private boolean selectable = DEFAULT_SELECTABLE;
    private boolean updateable = DEFAULT_UPDATEABLE;
    
    private DataType dataType = new DataType();
    
    /**
     * RelationalColumn constructor
     */
    public Column() {
        super();
    }
    
    /**
     * RelationalColumn constructor
     * @param name the name of the column
     */
    public Column( String name ) {
        super(name);
    }
    
    /**
     * @return type
     */
    @Override
	public int getType() {
        return TYPES.COLUMN;
    }

    /**
     * @return distinctValueCount
     */
    public int getDistinctValueCount() {
        return distinctValueCount;
    }
    /**
     * @param distinctValueCount Sets distinctValueCount to the specified value.
     */
    public void setDistinctValueCount( int distinctValueCount ) {
        this.distinctValueCount = distinctValueCount;
    }
    /**
     * @return nullValueCount
     */
    public int getNullValueCount() {
        return nullValueCount;
    }
    /**
     * @param nullValueCount Sets nullValueCount to the specified value.
     */
    public void setNullValueCount( int nullValueCount ) {
        this.nullValueCount = nullValueCount;
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
     * @return autoIncremented
     */
    public boolean isAutoIncremented() {
        return autoIncremented;
    }
    /**
     * @param autoIncremented Sets autoIncremented to the specified value.
     */
    public void setAutoIncremented( boolean autoIncremented ) {
        this.autoIncremented = autoIncremented;
    }
    /**
     * @return caseSensitive
     */
    public boolean isCaseSensitive() {
        return caseSensitive;
    }
    /**
     * @param caseSensitive Sets caseSensitive to the specified value.
     */
    public void setCaseSensitive( boolean caseSensitive ) {
        this.caseSensitive = caseSensitive;
    }
    /**
     * @return characterSetName
     */
    public String getCharacterSetName() {
        return characterSetName;
    }
    /**
     * @param characterSetName Sets characterSetName to the specified value.
     */
    public void setCharacterSetName( String characterSetName ) {
        this.characterSetName = characterSetName;
    }
    /**
     * @return collationName
     */
    public String getCollationName() {
        return collationName;
    }
    /**
     * @param collationName Sets collationName to the specified value.
     */
    public void setCollationName( String collationName ) {
        this.collationName = collationName;
    }
    /**
     * @return currency
     */
    public boolean isCurrency() {
        return currency;
    }
    /**
     * @param currency Sets currency to the specified value.
     */
    public void setCurrency( boolean currency ) {
        this.currency = currency;
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
     * @return format
     */
    public String getFormat() {
        return format;
    }
    /**
     * @param format Sets format to the specified value.
     */
    public void setFormat( String format ) {
        this.format = format;
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
     * @return lengthFixed
     */
    public boolean isLengthFixed() {
        return lengthFixed;
    }
    /**
     * @param lengthFixed Sets lengthFixed to the specified value.
     */
    public void setLengthFixed( boolean lengthFixed ) {
        this.lengthFixed = lengthFixed;
    }
    /**
     * @return maximumValue
     */
    public String getMaximumValue() {
        return maximumValue;
    }
    /**
     * @param maximumValue Sets maximumValue to the specified value.
     */
    public void setMaximumValue( String maximumValue ) {
        this.maximumValue = maximumValue;
    }
    /**
     * @return minimumValue
     */
    public String getMinimumValue() {
        return minimumValue;
    }
    /**
     * @param minimumValue Sets minimumValue to the specified value.
     */
    public void setMinimumValue( String minimumValue ) {
        this.minimumValue = minimumValue;
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
	 * @return the characterOctetLength
	 */
	public int getCharacterOctetLength() {
		return this.characterOctetLength;
	}

	/**
	 * @param characterOctetLength the characterOctetLength to set
	 */
	public void setCharacterOctetLength(int characterOctetLength) {
		this.characterOctetLength = characterOctetLength;
	}

	/**
     * @return signed
     */
    public boolean isSigned() {
        return signed;
    }
    /**
     * @param signed Sets signed to the specified value.
     */
    public void setSigned( boolean signed ) {
        this.signed = signed;
    }
    /**
     * @return searchability
     */
    public String getSearchability() {
        return searchability;
    }
    
    /**
     * @param searchability Sets searchability to the specified value.
     */
    public void setSearchability( String searchability ) {
    	ArgCheck.isNotEmpty(searchability);
    	String[] allowedValues = SEARCHABILITY.AS_ARRAY;
    	boolean matchFound = false;
    	for(int i=0; i<allowedValues.length; i++) {
    		if(allowedValues[i].equalsIgnoreCase(searchability)) {
    			this.searchability = allowedValues[i];
    			matchFound = true;
    		}
    	}
    	if(!matchFound) throw new IllegalArgumentException(Messages.getString(RELATIONAL.columnError_Searchability_NotAllowable,searchability));
    }

    /**
     * @return selectable
     */
    public boolean isSelectable() {
        return selectable;
    }
    /**
     * @param selectable Sets selectable to the specified value.
     */
    public void setSelectable( boolean selectable ) {
        this.selectable = selectable;
    }
    /**
     * @return updateable
     */
    public boolean isUpdateable() {
        return updateable;
    }
    /**
     * @param updateable Sets updateable to the specified value.
     */
    public void setUpdateable( boolean updateable ) {
        this.updateable = updateable;
    }
    
    /**
     * Get the properties for this object
     * @return the properties
     */
    @Override
	public Map<String,String> getProperties() {
    	Map<String,String> props = super.getProperties();
    	
    	props.put(KEY_AUTO_INCREMENTED, String.valueOf(isAutoIncremented()));
    	props.put(KEY_LENGTH, String.valueOf(getLength()));
    	props.put(KEY_CASE_SENSITIVE, String.valueOf(isCaseSensitive()));
    	props.put(KEY_CHARACTER_SET_NAME, getCharacterSetName());
    	props.put(KEY_CHARACTER_OCTET_LENGTH, String.valueOf(getCharacterOctetLength()));
    	props.put(KEY_COLLATION_NAME, getCollationName());
    	props.put(KEY_CURRENCY, String.valueOf(isCurrency()));
    	props.put(KEY_DATATYPE, getDatatypeName());
    	props.put(KEY_DEFAULT_VALUE, getDefaultValue());
    	props.put(KEY_DISTINCT_VALUE_COUNT, String.valueOf(getDistinctValueCount()));
    	props.put(KEY_FORMAT, getFormat());
    	props.put(KEY_LENGTH_FIXED, String.valueOf(isLengthFixed()));
    	props.put(KEY_MAXIMUM_VALUE, getMaximumValue());
    	props.put(KEY_MINIMUM_VALUE, getMinimumValue());
    	props.put(KEY_NATIVE_TYPE, getNativeType());
    	props.put(KEY_NULLABLE, getNullable());
    	props.put(KEY_NULL_VALUE_COUNT, String.valueOf(getPrecision()));
    	props.put(KEY_SCALE, String.valueOf(getScale()));
    	props.put(KEY_RADIX, String.valueOf(getRadix()));
    	props.put(KEY_SEARCHABILITY, getSearchability());
    	props.put(KEY_SELECTABLE, String.valueOf(isSelectable()));
    	props.put(KEY_SIGNED, String.valueOf(isSigned()));
    	props.put(KEY_UPDATEABLE, String.valueOf(isUpdateable()));
    	
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
            
            if(keyStr.equalsIgnoreCase(KEY_AUTO_INCREMENTED) ) {
                setAutoIncremented(Boolean.parseBoolean(value));
            } else if(keyStr.equalsIgnoreCase(KEY_LENGTH) ) {
                setLength(Integer.parseInt(value));
            } else if(keyStr.equalsIgnoreCase(KEY_CASE_SENSITIVE) ) {
                setCaseSensitive(Boolean.parseBoolean(value));
            } else if(keyStr.equalsIgnoreCase(KEY_CHARACTER_SET_NAME) ) {
                setCharacterSetName(value);
            } else if(keyStr.equalsIgnoreCase(KEY_CHARACTER_OCTET_LENGTH) ) {
            	setCharacterOctetLength(Integer.parseInt(value));
            } else if(keyStr.equalsIgnoreCase(KEY_COLLATION_NAME) ) {
                setCollationName(value);
            } else if(keyStr.equalsIgnoreCase(KEY_CURRENCY) ) {
                setCurrency(Boolean.parseBoolean(value));
            } else if(keyStr.equalsIgnoreCase(KEY_DATATYPE) ) {
                setDatatypeName(value);
            } else if(keyStr.equalsIgnoreCase(KEY_DEFAULT_VALUE) ) {
                setDefaultValue(value);
            } else if(keyStr.equalsIgnoreCase(KEY_DISTINCT_VALUE_COUNT) ) {
                setDistinctValueCount(Integer.parseInt(value));
            } else if(keyStr.equalsIgnoreCase(KEY_FORMAT) ) {
                setFormat(value);
            } else if(keyStr.equalsIgnoreCase(KEY_LENGTH_FIXED) ) {
                setLengthFixed(Boolean.parseBoolean(value));
            } else if(keyStr.equalsIgnoreCase(KEY_MAXIMUM_VALUE) ) {
                setMaximumValue(value);
            } else if(keyStr.equalsIgnoreCase(KEY_MINIMUM_VALUE) ) {
                setMinimumValue(value);
            } else if(keyStr.equalsIgnoreCase(KEY_NATIVE_TYPE) ) {
                setNativeType(value);
            } else if(keyStr.equalsIgnoreCase(KEY_NULLABLE) ) {
                setNullable(value);
            } else if(keyStr.equalsIgnoreCase(KEY_NULL_VALUE_COUNT) ) {
                setNullValueCount(Integer.parseInt(value));
            } else if(keyStr.equalsIgnoreCase(KEY_PRECISION) ) {
                setPrecision(Integer.parseInt(value));
            } else if(keyStr.equalsIgnoreCase(KEY_SCALE) ) {
                setScale(Integer.parseInt(value));
            } else if(keyStr.equalsIgnoreCase(KEY_RADIX) ) {
                setRadix(Integer.parseInt(value));
            } else if(keyStr.equalsIgnoreCase(KEY_SEARCHABILITY) ) {
                setSearchability(value);
            } else if(keyStr.equalsIgnoreCase(KEY_SELECTABLE) ) {
                setSelectable(Boolean.parseBoolean(value));
            } else if(keyStr.equalsIgnoreCase(KEY_SIGNED) ) {
                setSigned(Boolean.parseBoolean(value));
            } else if(keyStr.equalsIgnoreCase(KEY_UPDATEABLE) ) {
                setUpdateable(Boolean.parseBoolean(value));
            }
        }
    	
        handleInfoChanged();
    }
    
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(this.getClass().getName());
		sb.append(" : name = ").append(getName()); //$NON-NLS-1$
		return sb.toString();
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
        final Column other = (Column)object;

        // string properties
        if (!StringUtils.valuesAreEqualIgnoreCase(getCharacterSetName(), other.getCharacterSetName()) ||
        		!StringUtils.valuesAreEqualIgnoreCase(getCollationName(), other.getCollationName()) ||
        		!StringUtils.valuesAreEqualIgnoreCase(getDatatypeName(), other.getDatatypeName()) ||
        		!StringUtils.valuesAreEqualIgnoreCase(getDefaultValue(), other.getDefaultValue()) ||
        		!StringUtils.valuesAreEqualIgnoreCase(getFormat(), other.getFormat()) ||
        		!StringUtils.valuesAreEqualIgnoreCase(getMaximumValue(), other.getMaximumValue()) ||
        		!StringUtils.valuesAreEqualIgnoreCase(getMinimumValue(), other.getMinimumValue()) ||
        		!StringUtils.valuesAreEqualIgnoreCase(getNativeType(), other.getNativeType()) ||
        		!StringUtils.valuesAreEqualIgnoreCase(getNullable(), other.getNullable()) ||
        		!StringUtils.valuesAreEqualIgnoreCase(getSearchability(), other.getSearchability())  ) {
        	return false;
        }
        
        if( !(getDistinctValueCount()==other.getDistinctValueCount()) ||  
            !(getLength()==other.getLength()) ||
            !(getCharacterOctetLength()==other.getCharacterOctetLength()) ||
            !(getNullValueCount()==other.getNullValueCount()) ||
            !(getPrecision()==other.getPrecision()) ||
            !(getRadix()==other.getRadix()) ||
            !(getScale()==other.getScale()) ||
            !(isAutoIncremented()==other.isAutoIncremented()) ||
            !(isCaseSensitive()==other.isCaseSensitive()) ||
            !(isCurrency()==other.isCurrency()) ||
            !(isLengthFixed()==other.isLengthFixed()) ||
            !(isSelectable()==other.isSelectable()) ||
            !(isSigned()==other.isSigned()) ||
            !(isUpdateable()==other.isUpdateable()) ) {
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
        if (!StringUtils.isEmpty(getCharacterSetName())) {
            result = HashCodeUtils.hashCode(result, getCharacterSetName());
        }
        if (!StringUtils.isEmpty(getCollationName())) {
            result = HashCodeUtils.hashCode(result, getCollationName());
        }
        if (!StringUtils.isEmpty(getDatatypeName())) {
            result = HashCodeUtils.hashCode(result, getDatatypeName());
        }
        if (!StringUtils.isEmpty(getDefaultValue())) {
            result = HashCodeUtils.hashCode(result, getDefaultValue());
        }
        if (!StringUtils.isEmpty(getFormat())) {
            result = HashCodeUtils.hashCode(result, getFormat());
        }
        if (!StringUtils.isEmpty(getMaximumValue())) {
            result = HashCodeUtils.hashCode(result, getMaximumValue());
        }
        if (!StringUtils.isEmpty(getMinimumValue())) {
            result = HashCodeUtils.hashCode(result, getMinimumValue());
        }
        if (!StringUtils.isEmpty(getNativeType())) {
            result = HashCodeUtils.hashCode(result, getNativeType());
        }
        if (!StringUtils.isEmpty(getNullable())) {
            result = HashCodeUtils.hashCode(result, getNullable());
        }
        if (!StringUtils.isEmpty(getSearchability())) {
            result = HashCodeUtils.hashCode(result, getSearchability());
        }
        
        result = HashCodeUtils.hashCode(result, getDistinctValueCount());
        result = HashCodeUtils.hashCode(result, getLength());
        result = HashCodeUtils.hashCode(result, getCharacterOctetLength());
        result = HashCodeUtils.hashCode(result, getNullValueCount());
        result = HashCodeUtils.hashCode(result, getPrecision());
        result = HashCodeUtils.hashCode(result, getRadix());
        result = HashCodeUtils.hashCode(result, getScale());
        result = HashCodeUtils.hashCode(result, isAutoIncremented());
        result = HashCodeUtils.hashCode(result, isCaseSensitive());
        result = HashCodeUtils.hashCode(result, isCurrency());
        result = HashCodeUtils.hashCode(result, isLengthFixed());
        result = HashCodeUtils.hashCode(result, isSelectable());
        result = HashCodeUtils.hashCode(result, isSigned());
        result = HashCodeUtils.hashCode(result, isUpdateable());

        return result;
    }    
	
}
