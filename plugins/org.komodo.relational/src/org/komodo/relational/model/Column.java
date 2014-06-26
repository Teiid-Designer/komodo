/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import java.util.Properties;
import org.komodo.relational.core.RelationalStringNameValidator;
import org.komodo.spi.outcome.IOutcome;
import org.komodo.utils.HashCodeUtil;
import org.komodo.utils.StringUtil;



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
    public static final int DEFAULT_LENGTH = 0;
    @SuppressWarnings("javadoc")
    public static final boolean DEFAULT_LENGTH_FIXED = false;
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_MAXIMUM_VALUE = null;
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_MINIMUM_VALUE = null;
    @SuppressWarnings("javadoc")
    public static final int DEFAULT_PRECISION = 0;
    @SuppressWarnings("javadoc")
    public static final int DEFAULT_NUMERIC_PRECISION = 1;
    @SuppressWarnings("javadoc")
    public static final int DEFAULT_RADIX = 0;
    @SuppressWarnings("javadoc")
    public static final int DEFAULT_SCALE = 0;
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
    private String  datatype;
    private String  nativeType;
    private String  nullable = DEFAULT_NULLABLE;
    private boolean autoIncremented;
    private boolean caseSensitive = DEFAULT_CASE_SENSITIVE;
    private String  characterSetName;
    private String  collationName;
    private boolean currency;
    private String  defaultValue;
    private String  format;
    private int length;
    private boolean lengthFixed;
    private String  maximumValue;
    private String  minimumValue;
    private int precision;
    private int scale;
    private int radix;
    private int characterOctetLength;
	private boolean signed = DEFAULT_SIGNED;
    private String  searchability = DEFAULT_SEARCHABILITY;
    private boolean selectable = DEFAULT_SELECTABLE;
    private boolean updateable = DEFAULT_UPDATEABLE;
    
    /**
     * RelationalColumn constructor
     */
    public Column() {
        super();
        setNameValidator(new RelationalStringNameValidator(false));
    }
    
    /**
     * RelationalColumn constructor
     * @param name the name of the column
     */
    public Column( String name ) {
        super(name);
        setNameValidator(new RelationalStringNameValidator(false));
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
    public String getDatatype() {
        return datatype;
    }
    /**
     * @param datatype Sets datatype to the specified value.
     */
    public void setDatatype( String datatype ) {
        this.datatype = datatype;
        if( this.precision == DEFAULT_PRECISION &&
        	(this.datatype.equalsIgnoreCase("INTEGER") || //$NON-NLS-1$
        	this.datatype.equalsIgnoreCase("DECIMAL") || //$NON-NLS-1$
        	this.datatype.equalsIgnoreCase("LONG") || //$NON-NLS-1$
        	this.datatype.equalsIgnoreCase("SHORT") || //$NON-NLS-1$
        	this.datatype.equalsIgnoreCase("BIGDECIMAL") || //$NON-NLS-1$
        	this.datatype.equalsIgnoreCase("BIGINTEGER")) ) { //$NON-NLS-1$
        	setPrecision(DEFAULT_NUMERIC_PRECISION);
        }
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
        this.nullable = nullable;
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
    public int getLength() {
        return length;
    }
    /**
     * @param length Sets length to the specified value.
     */
    public void setLength( int length ) {
        this.length = length;
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
        return precision;
    }
    /**
     * @param precision Sets precision to the specified value.
     */
    public void setPrecision( int precision ) {
        this.precision = precision;
    }
    /**
     * @return scale
     */
    public int getScale() {
        return scale;
    }
    /**
     * @param scale Sets scale to the specified value.
     */
    public void setScale( int scale ) {
        this.scale = scale;
    }
    /**
     * @return radix
     */
    public int getRadix() {
        return radix;
    }
    /**
     * @param radix Sets radix to the specified value.
     */
    public void setRadix( int radix ) {
        this.radix = radix;
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
        this.searchability = searchability;
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
    
	@Override
	public IOutcome validate() {
		// Walk through the properties for the table and set the status
		return super.validate();
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
                setDatatype(value);
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
        if (!StringUtil.valuesAreEqualIgnoreCase(getCharacterSetName(), other.getCharacterSetName()) ||
        		!StringUtil.valuesAreEqualIgnoreCase(getCollationName(), other.getCollationName()) ||
        		!StringUtil.valuesAreEqualIgnoreCase(getDatatype(), other.getDatatype()) ||
        		!StringUtil.valuesAreEqualIgnoreCase(getDefaultValue(), other.getDefaultValue()) ||
        		!StringUtil.valuesAreEqualIgnoreCase(getFormat(), other.getFormat()) ||
        		!StringUtil.valuesAreEqualIgnoreCase(getMaximumValue(), other.getMaximumValue()) ||
        		!StringUtil.valuesAreEqualIgnoreCase(getMinimumValue(), other.getMinimumValue()) ||
        		!StringUtil.valuesAreEqualIgnoreCase(getNativeType(), other.getNativeType()) ||
        		!StringUtil.valuesAreEqualIgnoreCase(getNullable(), other.getNullable()) ||
        		!StringUtil.valuesAreEqualIgnoreCase(getSearchability(), other.getSearchability())  ) {
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
        if (!StringUtil.isEmpty(getCharacterSetName())) {
            result = HashCodeUtil.hashCode(result, getCharacterSetName());
        }
        if (!StringUtil.isEmpty(getCollationName())) {
            result = HashCodeUtil.hashCode(result, getCollationName());
        }
        if (!StringUtil.isEmpty(getDatatype())) {
            result = HashCodeUtil.hashCode(result, getDatatype());
        }
        if (!StringUtil.isEmpty(getDefaultValue())) {
            result = HashCodeUtil.hashCode(result, getDefaultValue());
        }
        if (!StringUtil.isEmpty(getFormat())) {
            result = HashCodeUtil.hashCode(result, getFormat());
        }
        if (!StringUtil.isEmpty(getMaximumValue())) {
            result = HashCodeUtil.hashCode(result, getMaximumValue());
        }
        if (!StringUtil.isEmpty(getMinimumValue())) {
            result = HashCodeUtil.hashCode(result, getMinimumValue());
        }
        if (!StringUtil.isEmpty(getNativeType())) {
            result = HashCodeUtil.hashCode(result, getNativeType());
        }
        if (!StringUtil.isEmpty(getNullable())) {
            result = HashCodeUtil.hashCode(result, getNullable());
        }
        if (!StringUtil.isEmpty(getSearchability())) {
            result = HashCodeUtil.hashCode(result, getSearchability());
        }
        
        result = HashCodeUtil.hashCode(result, getDistinctValueCount());
        result = HashCodeUtil.hashCode(result, getLength());
        result = HashCodeUtil.hashCode(result, getCharacterOctetLength());
        result = HashCodeUtil.hashCode(result, getNullValueCount());
        result = HashCodeUtil.hashCode(result, getPrecision());
        result = HashCodeUtil.hashCode(result, getRadix());
        result = HashCodeUtil.hashCode(result, getScale());
        result = HashCodeUtil.hashCode(result, isAutoIncremented());
        result = HashCodeUtil.hashCode(result, isCaseSensitive());
        result = HashCodeUtil.hashCode(result, isCurrency());
        result = HashCodeUtil.hashCode(result, isLengthFixed());
        result = HashCodeUtil.hashCode(result, isSelectable());
        result = HashCodeUtil.hashCode(result, isSigned());
        result = HashCodeUtil.hashCode(result, isUpdateable());

        return result;
    }    
	
}
